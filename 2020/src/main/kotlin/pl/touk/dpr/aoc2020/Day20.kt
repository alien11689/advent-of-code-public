package pl.touk.dpr.aoc2020

object Day20 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/20/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val tiles = readTiles(input)
        val borders = tiles.values.flatMap { it.borders }.groupBy { it }.mapValues { it.value.size }
        val uniqueBorders = borders.filter { it.value == 1 }.keys.toSet()
        val corners = tiles.values.filter { it.borders.count { it in uniqueBorders } > 3 }.map { it.id }
        return corners.fold(1L) { acc, l -> acc * l }
    }

    private fun readTiles(input: List<String>): Map<Long, Tile> {
        val tiles = mutableMapOf<Long, Tile>()
        var i = 0
        var curTile = mutableListOf<List<Char>>()
        var curNum = -1L
        while (i < input.size) {
            if (i % 11 == 0) {
                if (curNum > -1) {
                    tiles[curNum] = Tile(curNum, curTile)
                }
                curNum = input[i].split(Regex("[: ]"))[1].toLong()
                curTile = mutableListOf()
            } else {
                curTile.add(input[i].toCharArray().toList())
            }
            ++i
        }
        tiles[curNum] = Tile(curNum, curTile)
        return tiles.toMap()
    }

    private fun part2(input: List<String>): Any {
        val tiles = readTiles(input).toMutableMap()
        val whole = buildWholeImage(tiles)
        return findRoughWaterWithoutDragons(whole)
    }

    private fun findRoughWaterWithoutDragons(wholeInit: Tile): Int {
        var whole = wholeInit
        var i = 0
        while (true) {
            val foundDragons = lookForDragon(whole)
            if (foundDragons > 0) {
                val all = whole.image.map { it.filter { it == '#' }.count() }.sum()
                return all - foundDragons * dragonPoints().size
            }
            whole = if (i % 4 == 3) whole.transpose() else whole.rotate()
            ++i
        }
    }

    private fun buildWholeImage(tiles: MutableMap<Long, Tile>): Tile {
        val borders = tiles.values.flatMap { it.borders }.groupBy { it }.mapValues { it.value.size }
        val uniqueBorders = borders.filter { it.value == 1 }.keys.toSet()
        val wholeImage: MutableList<MutableList<Tile?>> = IntRange(0, 11).map {
            IntRange(0, 11).map {
                null
            }.toMutableList<Tile?>()
        }.toMutableList()
        wholeImage.forEachIndexed { rowIdx, _ ->
            wholeImage.forEachIndexed { colIdx, _ ->
                var matching: Tile
                if (colIdx != 0) {
                    val left = wholeImage[rowIdx][colIdx - 1]!!
                    if (rowIdx == 0) {
                        matching = tiles.values.find { cur -> left.column(9) in cur.borders && uniqueBorders.any { it in cur.borders } }!!
                        var i = 0
                        while (matching.row(0) !in uniqueBorders || matching.column(0) != left.column(9)) {
                            matching = if (i % 4 == 3) matching.transpose() else matching.rotate()
                            ++i
                        }
                    } else {
                        val upper = wholeImage[rowIdx - 1][colIdx]!!
                        matching = tiles.values.find { cur -> left.column(9) in cur.borders && upper.row(9) in cur.borders }!!
                        var i = 0
                        while (matching.row(0) != upper.row(9) || matching.column(0) != left.column(9)) {
                            matching = if (i % 4 == 3) matching.transpose() else matching.rotate()
                            ++i
                        }
                    }
                } else {
                    // colIdx == 0
                    if (rowIdx != 0) {
                        val upper = wholeImage[rowIdx - 1][colIdx]!!
                        matching = tiles.values.find { cur -> upper.row(9) in cur.borders && uniqueBorders.any { it in cur.borders } }!!
                        var i = 0
                        while (matching.column(0) !in uniqueBorders || matching.row(0) != upper.row(9)) {
                            matching = if (i % 4 == 3) matching.transpose() else matching.rotate()
                            ++i
                        }
                    } else {
                        matching = tiles.values.find { cur -> uniqueBorders.count { it in cur.borders } > 3 }!!
                        var i = 0
                        while (matching.column(0) !in uniqueBorders || matching.row(0) !in uniqueBorders) {
                            matching = if (i % 4 == 3) matching.transpose() else matching.rotate()
                            ++i
                        }
                    }
                }
                wholeImage[rowIdx][colIdx] = matching
                tiles.remove(matching.id)
            }
        }
        return Tile(-1L, join(wholeImage))
    }

    private fun lookForDragon(whole: Tile): Int {
        val dragon = dragonPoints()
        var found = 0
        (0..(whole.image.size - 3)).forEach { rowId ->
            (0..(whole.image[rowId].size - 20)).forEach { colId ->
                if (dragon.map { p -> Pair(p.first + rowId, p.second + colId) }.all { whole.image[it.first][it.second] == '#' }) {
                    ++found
                }
            }
        }
        return found
    }

    private fun dragonPoints(): Set<Pair<Int, Int>> {
        //                  #
        //#    ##    ##    ###
        // #  #  #  #  #  #
        return setOf(
                Pair(0, 18),
                Pair(1, 0),
                Pair(1, 5),
                Pair(1, 6),
                Pair(1, 11),
                Pair(1, 12),
                Pair(1, 17),
                Pair(1, 18),
                Pair(1, 19),
                Pair(2, 1),
                Pair(2, 4),
                Pair(2, 7),
                Pair(2, 10),
                Pair(2, 13),
                Pair(2, 16)
        )
    }

    private fun join(wholeImage: List<List<Tile?>>): List<List<Char>> {
        val ml = mutableListOf<List<Char>>()
        wholeImage.forEach { rowTiles ->
            (0..7).forEach { insideRow ->
                val newRow = mutableListOf<Char>()
                rowTiles.forEach { tile ->
                    newRow.addAll(tile!!.withoutBorders().row(insideRow))
                }
                ml.add(newRow.toList())
            }
        }
        return ml.toList()
    }

    data class Tile(val id: Long, val image: List<List<Char>>) {
        val borders: List<List<Char>> = listOf(
                image[0],
                image[image.size - 1],
                column(0),
                column(image[0].size - 1),
                image[0].reversed(),
                image[image.size - 1].reversed(),
                column(0).reversed(),
                column(image[0].size - 1).reversed(),
        )

        fun column(col: Int): List<Char> {
            val ml = mutableListOf<Char>()
            image.forEach { ml.add(it[col]) }
            return ml.toList()
        }

        fun row(r: Int): List<Char> {
            return image[r]
        }

        fun transpose(): Tile {
            val newImage = IntRange(0, image.size - 1).map {
                column(it)
            }
            return copy(image = newImage)
        }

        fun rotate(): Tile {
            return copy(image = IntRange(0, image.size - 1).map {
                column(it).reversed()
            })
        }

        fun print() {
            image.forEach {
                it.forEach { print(it) }
                println()
            }
        }

        fun withoutBorders(): Tile {
            val newImage = image.mapIndexed { rowIdx: Int, row: List<Char> ->
                if (rowIdx == 0 || rowIdx == 9) {
                    listOf()
                } else {
                    row.flatMapIndexed { colIdx: Int, col: Char ->
                        if (colIdx == 0 || colIdx == 9) {
                            listOf()
                        } else {
                            listOf(col)
                        }
                    }
                }
            }.filter { it.isNotEmpty() }
            return copy(image = newImage)
        }
    }
}
