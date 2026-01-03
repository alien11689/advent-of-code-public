package dpr.aoc2021

import dpr.commons.Util

object Day01 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/01/input.txt")
        val numbers = parseInput(input)

        println(part1(numbers))
        println(part2(numbers))
    }

    @JvmStatic
    fun parseInput(input: List<String>): List<Int> = input.map { it.toInt() }

    @JvmStatic
    fun part1(numbers: List<Int>): Int {
        return numbers.zipWithNext().count { it.first < it.second }
    }

    @JvmStatic
    fun part2(numbers: List<Int>): Int {
        return numbers.windowed(3, 1).map { it.sum() }.zipWithNext().count { it.first < it.second }
    }
}

