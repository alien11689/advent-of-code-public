package dpr.aoc2020

object Day24 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/24/input.txt")
//        val input = Util.getNotEmptyLinesFromFile("/24/sample.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val blackTiles = readInitialTiles(input)
        return blackTiles.size
    }

    private fun readInitialTiles(input: List<String>): MutableSet<Tile> {
        val blackTiles = mutableSetOf<Tile>()
        input.forEach { line ->
            val tile = goTo(line.toCharArray())
            if (tile in blackTiles) {
                blackTiles.remove(tile)
            } else {
                blackTiles.add(tile)
            }
        }
        return blackTiles
    }

    private fun goTo(instr: CharArray): Tile {
        var cur = Tile(0, 0)
        var i = 0
        while (i < instr.size) {
            when (val c = instr[i]) {
                'e' -> cur = cur.east()
                'w' -> cur = cur.west()
                'n' -> {
                    ++i
                    cur = when (val nextC = instr[i]) {
                        'w' -> cur.northwest()
                        'e' -> cur.northeast()
                        else -> throw RuntimeException("$c$nextC")
                    }
                }
                's' -> {
                    ++i
                    cur = when (val nextC = instr[i]) {
                        'w' -> cur.southwest()
                        'e' -> cur.southeast()
                        else -> throw RuntimeException("$c$nextC")
                    }
                }
                else -> throw RuntimeException("$c")
            }
            ++i
        }
        return cur
    }

    private fun part2(input: List<String>): Any {
        var blackTiles: Set<Tile> = readInitialTiles(input)
        repeat(100) {
            blackTiles = blackTiles.flatMap { it.neighbours() }.toSet().flatMap { curTile ->
                val adj = curTile.neighbours().count { it in blackTiles }
                if (curTile in blackTiles) {
                    when (adj) {
                        0 -> listOf()
                        1 -> listOf(curTile)
                        2 -> listOf(curTile)
                        else -> listOf()
                    }
                } else {
                    when (adj) {
                        2 -> listOf(curTile)
                        else -> listOf()
                    }
                }
            }.toSet()
        }
        return blackTiles.size
    }

    data class Tile(val x: Int, val y: Int) {
        fun east(): Tile = copy(x = x + 1)
        fun west(): Tile = copy(x = x - 1)
        fun northeast(): Tile = copy(x = x + 1, y = y - 1)
        fun southwest(): Tile = copy(x = x - 1, y = y + 1)
        fun northwest(): Tile = copy(y = y - 1)
        fun southeast(): Tile = copy(y = y + 1)
        fun neighbours(): Set<Tile> = setOf(
                east(),
                west(),
                northeast(),
                northwest(),
                southeast(),
                southwest()
        )
    }
}
