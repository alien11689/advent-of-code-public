package dpr.aoc2020

import dpr.commons.Util

object Day09 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/09/input.txt")
            .map { it.toLong() }
        val solutionPart1 = part1(input)
        println(solutionPart1)
        println(part2(input, solutionPart1))
    }

    @JvmStatic
    fun part1(input: List<Long>): Long {
        var i = 25
        while (i < input.size) {
            val nums: List<Long> = input.subList(i - 25, i).sorted()
            val expected = input[i]
            var j = 0
            val checked = mutableSetOf<Long>()
            var found = false
            while (j < nums.size - 1) {
                if (nums[j] in checked) {
                    ++j
                    continue
                }
                checked.add(nums[j])
                val target = expected - nums[j]
                ++j
                if (target in nums) {
                    found = true
                    break
                }
            }
            if (!found) {
                return expected
            }
            ++i
        }
        throw RuntimeException("No solution for part 1")
    }

    @JvmStatic
    fun part2(input: List<Long>, targetValue: Long): Long {
        var i = 0
        var j = 1
        while (j < input.size) {
            val sublist = input.subList(i, j + 1)
            val sum = sublist.sum()
            if (sum == targetValue) {
                return sublist.maxOrNull()!! + sublist.minOrNull()!!
            } else if (sum < targetValue) {
                ++j
            } else {
                ++i
            }
            if (j == i) {
                throw RuntimeException("Set size cannot be 1")
            }
        }
        throw RuntimeException("No solution")
    }
}
