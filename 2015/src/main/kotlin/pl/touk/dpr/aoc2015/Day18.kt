package pl.touk.dpr.aoc2015

object Day18 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/18/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        var points = input.map { line ->
            line.map { it == '#' }
        }
        (1..100).forEach {
            points = points.mapIndexed { y, line ->
                line.mapIndexed { x, on ->
                    val onNeighbours = neighbours(x, y).count { points[it.second][it.first] }
                    on && onNeighbours in setOf(2, 3) || !on && onNeighbours == 3
                }
            }
        }
        return points.flatten().count { it }
    }

    fun neighbours(x: Int, y: Int): Set<Pair<Int, Int>> =
            listOf(
                    Pair(x + 1, y),
                    Pair(x + 1, y + 1),
                    Pair(x + 1, y - 1),
                    Pair(x - 1, y - 1),
                    Pair(x - 1, y),
                    Pair(x - 1, y + 1),
                    Pair(x, y - 1),
                    Pair(x, y + 1),
            )
                    .filter { it.first >= 0 && it.first < 100 && it.second >= 0 && it.second < 100 }
                    .toSet()

    private fun part2(input: List<String>): Any {
        var points = input.mapIndexed { y, line ->
            line.mapIndexed { x, c -> c == '#' || x in setOf(0, 99) && y in setOf(0, 99) }
        }
        (1..100).forEach {
            points = points.mapIndexed { y, line ->
                line.mapIndexed { x, on ->
                    val onNeighbours = neighbours(x, y).count { points[it.second][it.first] }
                    x in setOf(0, 99) && y in setOf(0, 99) || on && onNeighbours in setOf(2, 3) || !on && onNeighbours == 3
                }
            }
        }
        return points.flatten().count { it }
    }
}