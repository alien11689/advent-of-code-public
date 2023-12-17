package dpr.aoc2021

import dpr.commons.Util

object Day01 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val numbers = Util.getNotEmptyLinesFromFile("/01/input.txt")
            .map { it.toInt() }

        println(part1(numbers))
        println(part2(numbers))
    }

    private fun part1(numbers: List<Int>): Int {
        return numbers.zipWithNext().count { it.first < it.second }
    }

    private fun part2(numbers: List<Int>): Int {
        return numbers.windowed(3, 1).map { it.sum() }.zipWithNext().count { it.first < it.second }
    }
}

