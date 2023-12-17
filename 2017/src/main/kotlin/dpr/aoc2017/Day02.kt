package dpr.aoc2017

import dpr.commons.Util

object Day02 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/02/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        return input
            .map { line -> line.split('\t').map { it.toInt() } }
            .sumOf { it.max() - it.min() }
    }

    private fun part2(input: List<String>): Any {
        return input
            .map { line -> line.split('\t').map { it.toInt() }.sorted().reversed() }
            .sumOf { findDivisible(it) }
    }

    private fun findDivisible(nums: List<Int>): Int {
        for (i in nums.indices) {
            val first = nums[i]
            for (j in ((i + 1) until nums.size)) {
                if (first % nums[j] == 0) {
                    return first / nums[j]
                }
            }
        }
        return 0
    }

}
