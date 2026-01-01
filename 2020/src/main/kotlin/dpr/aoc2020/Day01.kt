package dpr.aoc2020

import dpr.commons.Util

object Day01 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/01/input.txt")
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: List<String>): Int {
        val mem = mutableSetOf<Int>()
        return input
            .flatMap { s ->
                val n = s.toInt()
                val compliant = 2020 - n
                if (compliant in mem) {
                    listOf(n * compliant)
                } else {
                    mem.add(n)
                    listOf()
                }
            }
            .take(1)[0]
    }

    @JvmStatic
    fun part2(input: List<String>): Int {
        val nums = input.map { it.toInt() }
        for ((i, a) in nums.withIndex()) {
            for ((ii, b) in nums.withIndex().filter { it.index > i }) {
                for ((_, c) in nums.withIndex().filter { it.index > ii }) {
                    if (a + b + c == 2020) {
                        return a * b * c
                    }
                }
            }
        }
        throw RuntimeException("No solution")
    }

}

