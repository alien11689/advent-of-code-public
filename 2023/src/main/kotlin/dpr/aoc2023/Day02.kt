package dpr.aoc2023

import dpr.commons.Util

object Day02 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/02/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private val limits = mapOf(
        "green" to 13,
        "red" to 12,
        "blue" to 14,
    )

    private fun part1(lines: List<String>): Any {
        return lines.sumOf { line ->
            val (header, rest) = line.split(": ")
            val rounds = rest.split("; ")
            val gameId = header.split(" ")[1].toInt()
            val anyRoundExceedsLimits = rounds.any { round ->
                val m = round.split(Regex("[ ,]+")).chunked(2) { l -> l[1] to l[0].toInt() }.toMap()
                limits.any { (color, limit) -> (m[color] ?: 0) > limit }
            }
            if (anyRoundExceedsLimits) 0 else gameId
        }
    }

    private fun part2(lines: List<String>): Any {
        return lines.sumOf { line ->
            val rest = line.split(": ")[1]
            val rounds = rest.split("; ")
            val max = limits.mapValues { 1 }.toMutableMap()
            rounds.forEach { round ->
                val m = round.split(Regex("[ ,]+")).chunked(2) { l -> l[1] to l[0].toInt() }.toMap()
                max.keys.forEach { color ->
                    if ((m[color] ?: 0) > max[color]!!) {
                        max[color] = m[color]!!
                    }
                }
            }
            max.values.fold(1L) { acc, colorValue -> acc * colorValue }
        }
    }
}

