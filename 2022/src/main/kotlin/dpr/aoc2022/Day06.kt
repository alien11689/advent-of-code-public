package dpr.aoc2022

import dpr.commons.Util

object Day06 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/06/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    @JvmStatic
    fun part1(lines: List<String>): Int {
        return getFirstIndexAfterDistinctSequence(lines, 4)
    }

    @JvmStatic
    fun part2(lines: List<String>): Int {
        return getFirstIndexAfterDistinctSequence(lines, 14)
    }

    private fun getFirstIndexAfterDistinctSequence(lines: List<String>, distinctSize: Int): Int {
        val signal = lines.first().toList()
        return distinctSize + signal.windowed(distinctSize, 1)
            .indexOfFirst { it.toSet().size == distinctSize }
    }
}

