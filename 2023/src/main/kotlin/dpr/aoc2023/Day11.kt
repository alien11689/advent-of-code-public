package dpr.aoc2023

import kotlin.math.max
import kotlin.math.min

object Day11 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/11/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/11/test1.txt")
        val (part1, part2) = part1And2(lines)
        println(part1)
        println(part2)
    }

    private fun part1And2(lines: List<String>): Pair<Long, Long> {
        val expandFactor1 = 2
        val expandFactor2 = 1_000_000
        val galaxies = mutableSetOf<Point2D>()
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                if (c == '#') {
                    galaxies.add(Point2D(x, y))
                }
            }
        }
        val emptyX = (galaxies.minOf { it.x }..(galaxies.maxOf { it.x })).filter { x -> galaxies.none { it.x == x } }
        val emptyY = (galaxies.minOf { it.y }..(galaxies.maxOf { it.y })).filter { y -> galaxies.none { it.y == y } }

        val pairs = mutableSetOf<Set<Point2D>>()
        var lengths1 = 0L
        var lengths2 = 0L
        for (g1 in galaxies) {
            for (g2 in galaxies) {
                if (g1 != g2 && g1.x <= g2.x) {
                    val pair = setOf(g1, g2)
                    if (pair !in pairs) {
                        pairs.add(pair)
                        val minX = min(g1.x, g2.x)
                        val maxX = max(g1.x, g2.x)
                        val minY = min(g1.y, g2.y)
                        val maxY = max(g1.y, g2.y)
                        val manhattanDistance = g1.manhattan(g2)
                        val expandAddition = emptyX.count { it in (minX + 1)..<maxX } + emptyY.count { it in (minY + 1)..<maxY }
                        lengths1 += manhattanDistance + expandAddition * (expandFactor1 - 1)
                        lengths2 += manhattanDistance + expandAddition * (expandFactor2 - 1)
                    }
                }
            }
        }
        return lengths1 to lengths2
    }
}

