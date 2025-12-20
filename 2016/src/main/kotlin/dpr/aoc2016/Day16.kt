package dpr.aoc2016

import dpr.commons.Util

object Day16 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = "00111101111101000"
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic fun part1(input: String, size : Int = 272): String {
        return solve(input, size)
    }

    private fun solve(input: String, size: Int): String {
        var text = input.toList()
        while (text.size < size) {
            text = step(text)
        }
        return checksum(text.take(size)).joinToString("")
    }

    private fun step(a: List<Char>): List<Char> {
        val b = a.reversed().map { if (it == '0') '1' else '0' }
        return a + '0' + b
    }

    private fun checksum(init: List<Char>): List<Char> {
        var text = init
        while (text.size % 2 == 0) {
            text = text.chunked(2).map { if (it[0] == it[1]) '1' else '0' }
        }
        return text
    }

    private fun part2(input: String): String {
        return solve(input, 35651584)
    }
}
