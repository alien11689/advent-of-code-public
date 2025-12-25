package dpr.aoc2017

import dpr.commons.Util

object Day15 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        println(part1())
        println(part2())
    }

    @JvmStatic
    fun part1(initA: Long = 883L, initB: Long = 879L): Int {
        var genA = initA
        var genB = initB

        val factorA = 16807
        val factorB = 48271

        val div = 2147483647

        var i = 0
        var judge = 0
        while (i < 40000000) {
            genA = genA * factorA % div
            genB = genB * factorB % div
            val a = genA % 65536
            val b = genB % 65536
            if (a == b) {
                ++judge
            }
            ++i
        }
        return judge
    }

    @JvmStatic
    fun part2(initA: Long = 883L, initB: Long = 879L): Int {
        var genA = initA
        var genB = initB

        val factorA = 16807
        val factorB = 48271

        var i = 0
        var judge = 0

        val divA = 4
        val divB = 8

        while (i < 5000000) {
            genA = nextAccept(genA, factorA, divA)
            genB = nextAccept(genB, factorB, divB)
            val a = genA % 65536
            val b = genB % 65536
            if (a == b) {
                ++judge
            }
            ++i
        }
        return judge
    }

    private fun next(prev: Long, factor: Int): Long {
        return prev * factor % 2147483647
    }

    private fun nextAccept(prev: Long, factor: Int, divider: Int): Long {
        var nextV = next(prev, factor)
        while (nextV % divider != 0L) {
            nextV = next(nextV, factor)
        }
        return nextV
    }
}
