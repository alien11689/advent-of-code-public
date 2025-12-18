package dpr.aoc2016

import dpr.commons.Util

object Day06 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/06/input.txt")
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: List<String>): String {
        return (0 until input[0].length).map { col ->
            input.map { it[col] }
                .groupingBy { it }
                .eachCount()
                .maxByOrNull { it.value }!!
                .key
        }.joinToString("")
    }

    @JvmStatic
    fun part2(input: List<String>): String {
        return (0 until input[0].length).map { col ->
            input.map { it[col] }
                .groupingBy { it }
                .eachCount()
                .minByOrNull { it.value }!!
                .key
        }.joinToString("")
    }
}
