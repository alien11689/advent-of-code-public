package dpr.aoc2019

import dpr.aoc2019.intcode.IntCodeComputer
import dpr.commons.Util

object Day19 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/19/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val m = mutableMapOf<Pair<Long, Long>, Long>()
        for (j in 0L until 50) {
            for (i in 0L until 50) {
                val out = IntCodeComputer.run(input, listOf(i, j))
                m[i to j] = out.first()
            }
        }
        return m.values.sum()
    }

    private fun part2(input: String): Any {
        val j = 10L
        val i = 0L
        var last = findLast(input, i, j)

        while (true) {
            if (IntCodeComputer.run(input, listOf(last.first, last.second + 99L)).first() == 1L &&
                IntCodeComputer.run(input, listOf(last.first - 99, last.second + 99)).first() == 1L
            ) {
//                println("Found: ${(last.x - 99) * 10000 + last.y}")
                return (last.first - 99) * 10000 + last.second
            }
            last = findLast(input, last.first, last.second + 1)
        }
    }

    private fun findLast(input: String, initI: Long, j: Long): Pair<Long, Long> {
        var i = initI
        var v = IntCodeComputer.run(input, listOf(i, j)).first()
        while (true) {
            val newVal = IntCodeComputer.run(input, listOf(i + 1, j)).first()
            if (v == 1L && newVal == 0L) {
                return i to j
            }
            v = newVal
            ++i
        }
    }
}
