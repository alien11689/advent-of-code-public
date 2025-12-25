package dpr.aoc2017

import dpr.commons.Util

object Day17 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        println(part1())
        println(part2())
    }

    @JvmStatic
    fun part1(input: Int = 344): Int {
        val buf = mutableListOf(0)
        var cur = 0

        (1..2017).forEach { i ->
            var curInput = input % buf.size
            while (curInput > 0) {
                cur = (cur + 1) % buf.size
                --curInput
            }
            buf.add(++cur, i)
        }
        return buf[cur + 1]
    }

    @JvmStatic
    fun part2(): Int {
        val input = 344
        val amount = 50000000

        var size = 1
        var cur = 0
        var nextToZero = -1

        (1..amount).forEach { i ->
            val curInput = input % size
            cur = (cur + curInput) % size
            ++cur
            ++size
            if (cur == 1) {
                nextToZero = i
            }
        }
        return nextToZero
    }
}
