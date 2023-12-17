package dpr.aoc2015

import dpr.commons.Util

object Day25 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        println(part1())
    }

    private fun part1(): Any {
        val column = 3083
        val row = 2978
        var i = 1 //row
        var j = 1 //column
        var cur = 20151125L

        while (i != row || j != column) {
            cur = next(cur)
            val res = nextIdx(i, j)
            i = res.first
            j = res.second
        }
        return cur
    }

    private fun next(cur: Long): Long {
        return cur * 252533 % 33554393
    }

    private fun nextIdx(i: Int, j: Int): Pair<Int, Int> {
        return if (i == 1 && j == 1) {
            Pair(i + 1, j)
        } else if (i == 1) {
            Pair(j + 1, 1)
        } else {
            Pair(i - 1, j + 1)
        }
    }
}
