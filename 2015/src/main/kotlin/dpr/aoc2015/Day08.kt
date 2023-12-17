package dpr.aoc2015

import dpr.commons.Util

object Day08 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/08/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Int {
        return input.sumOf { it.length - countStringChars(it) }
    }

    private fun countStringChars(s: String): Int {
        var i = 1
        var count = 0
        var escaped = false
        while (i < s.length - 1) {
            if (s[i] == '\\') {
                escaped = if (escaped) {
                    ++count
                    false
                } else {
                    true
                }
                ++i
            } else if (escaped) {
                ++count
                if (s[i] == 'x') {
                    i += 3
                } else {
                    ++i
                }
                escaped = false
            } else {
                ++count
                ++i
            }
        }
        return count
    }

    private fun part2(input: List<String>): Any {
        return input.sumOf { escapedStringLength(it) - it.length }
    }

    private fun escapedStringLength(s: String): Int {
        return s.fold(2) { acc, c ->
            acc + when (c) {
                '\\' -> 2
                '"' -> 2
                else -> 1
            }
        }
    }
}
