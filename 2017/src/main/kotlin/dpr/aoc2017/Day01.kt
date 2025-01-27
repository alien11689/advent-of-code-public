package dpr.aoc2017

import dpr.commons.Util

object Day01 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/01/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        return input.mapIndexed { index, s ->
            val next = input[(index + 1) % input.length]
            if (next == s) s.toString().toInt() else 0
        }.sum()
    }

    private fun part2(input: String): Any {
        return input.mapIndexed { index, s ->
            val next = input[(index + input.length / 2) % input.length]
            if (next == s) s.toString().toInt() else 0
        }.sum()
    }
}
