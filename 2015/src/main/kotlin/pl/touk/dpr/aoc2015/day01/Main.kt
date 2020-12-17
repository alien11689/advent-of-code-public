package pl.touk.dpr.aoc2015.day01

import pl.touk.dpr.aoc2015.Util
import java.lang.RuntimeException

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getFileContent("/01/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        return input.fold(0) { acc, c ->
            if (c == '(') {
                acc + 1
            } else if (c == ')') {
                acc - 1
            } else {
                acc
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