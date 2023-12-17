package dpr.aoc2017

import dpr.commons.Util

object Day05 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/05/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val steps = input.map { it.toInt() }.toMutableList()
        var cur = 0
        var jumps = 0
        while (cur < steps.size) {
            jumps++
            val next = steps[cur]
            steps[cur] = next + 1
            cur += next
        }
        return jumps
    }

    private fun part2(input: List<String>): Any {
        val steps = input.map { it.toInt() }.toMutableList()
        var cur = 0
        var jumps = 0
        while (cur < steps.size) {
            jumps++
            val next = steps[cur]
            steps[cur] = next + if (next >= 3) -1 else 1
            cur += next
        }
        return jumps
    }
}
