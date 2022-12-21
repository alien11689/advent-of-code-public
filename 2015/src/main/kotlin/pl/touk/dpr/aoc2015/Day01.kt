package pl.touk.dpr.aoc2015

import java.lang.RuntimeException

object Day01 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/01/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        return input.fold(0) { acc, c ->
            when (c) {
                '(' -> acc + 1
                ')' -> acc - 1
                else -> acc
            }
        }
    }

    private fun part2(input: String): Any {
        var floor = 0
        var pos = 0
        for (s in input) {
            floor += if (s == '(') 1 else -1
            ++pos
            if (floor == -1) {
                return pos
            }
        }
        throw RuntimeException("No Solution")
    }
}
