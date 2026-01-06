package dpr.aoc2023

import dpr.commons.Util

object Day09 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/09/input.txt")
        val (part1, part2) = part1And2(lines)
        println(part1)
        println(part2)
    }

    @JvmStatic
    fun part1And2(lines: List<String>): Pair<Long, Long> {
        val results = lines.map { line ->
            val values = line.split(" ").map { it.toLong() }
            findPrevAndNextValue(values)
        }
        return Pair(
            results.sumOf { it.second },
            results.sumOf { it.first },
        )
    }

    private fun findPrevAndNextValue(values: List<Long>): Pair<Long, Long> {
        if (values.toSet().size == 1) {
            return Pair(values[0], values[0])
        } else {
            val lowerList = values.windowed(2, 1, false) { it[1] - it[0] }
            val (decrement, increment) = findPrevAndNextValue(lowerList)
            return Pair(values.first() - decrement, values.last() + increment)
        }
    }
}

