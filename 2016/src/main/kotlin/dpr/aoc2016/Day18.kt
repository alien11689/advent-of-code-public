package dpr.aoc2016

import dpr.commons.Util

object Day18 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = ".^^^.^.^^^^^..^^^..^..^..^^..^.^.^.^^.^^....^.^...^.^^.^^.^^..^^..^.^..^^^.^^...^...^^....^^.^^^^^^^"
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val size = 40
        return solve(input, size)
    }

    private fun solve(input: String, size: Int): Int {
        var prev = input.toList()
        var count = 1
        var safe = prev.count { it == '.' }
        while (count < size) {
            val begin = listOf('.') + prev + listOf('.')
            prev = (begin.windowed(3, 1).filter { it.size == 3 }
                .map { if (isTrap(it[0], it[1], it[2])) '^' else '.' })
            safe += prev.count { it == '.' }
            ++count
        }
        return safe
    }

    private fun part2(input: String): Any {
        val size = 400000
        return solve(input, size)
    }

    private fun isTrap(l: Char, c: Char, r: Char): Boolean {
        return l == '^' && c == '^' && r == '.' ||
            l == '.' && c == '^' && r == '^' ||
            l == '.' && c == '.' && r == '^' ||
            l == '^' && c == '.' && r == '.'
    }
}
