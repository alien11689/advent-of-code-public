package dpr.aoc2023

import dpr.commons.Util
import kotlin.math.max
import kotlin.math.min

object Day11 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/11/input.txt")
        val (part1, part2) = part1And2(lines)
        println(part1)
        println(part2)
    }

    @JvmStatic
    fun part1And2(lines: List<String>, expandFactor2: Int = 1_000_000): Pair<Long, Long> {
        val expandFactor1 = 2
        val galaxies = Util.readBoard(lines).filter { it.value == '#' }.keys.sorted()
        val emptyX = (galaxies.minOf { it.x }..(galaxies.maxOf { it.x })).filter { x -> galaxies.none { it.x == x } }
        val emptyY = (galaxies.minOf { it.y }..(galaxies.maxOf { it.y })).filter { y -> galaxies.none { it.y == y } }

        var lengths1 = 0L
        var lengths2 = 0L
        for (i in galaxies.indices) {
            for (j in (i + 1)..<(galaxies.size)) {
                val g1 = galaxies[i]
                val g2 = galaxies[j]
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
        return lengths1 to lengths2
    }
}

