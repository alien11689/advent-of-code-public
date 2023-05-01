package dpr.aoc2015

import kotlin.math.sqrt

object Day20 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = 29000000
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: Int): Any {
        var cur = 1
        while (true) {
            val res = divisors(cur).sumOf { it * 10 }
            if (res >= input) {
                return cur
            }
            ++cur
        }
    }

    private fun divisors(n: Int): Set<Int> {
        val ms = mutableSetOf(1, n)
        var i = 2
        val limit = sqrt(n.toDouble())
        while (i < limit) {
            if (n % i == 0) {
                ms.add(i)
                ms.add(n / i)
            }
            ++i
        }
        return ms
    }

    private fun part2(input: Int): Any {
        var cur = 1
        while (true) {
            val res = divisors(cur).filter { it * 50 >= cur }.sumOf { it * 11 }
            if (res >= input) {
                return cur
            }
            ++cur
        }
    }
}
