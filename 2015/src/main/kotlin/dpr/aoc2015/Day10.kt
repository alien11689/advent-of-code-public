package dpr.aoc2015

import dpr.commons.Util

object Day10 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = "1321131112"
        part1And2(input)
    }

    private fun part1And2(input: String) {
        var cur = input.toList()
        val interestingIterations = setOf(40, 50)
        (1..50).forEach { iter ->
            cur = next(cur)
            if (iter in interestingIterations) {
                println(cur.size)
            }
        }
    }

    private fun next(input: List<Char>): List<Char> {
        val newList = mutableListOf<Char>()
        var cur: Char? = null
        var count = 0
        var i = 0
        while (i < input.size) {
            if (input[i] == cur) {
                ++count
            } else {
                if (cur != null) {
                    newList.addAll(count.toString().toList())
                    newList.add(cur)
                }
                cur = input[i]
                count = 1
            }
            ++i
        }
        newList.addAll(count.toString().toList())
        newList.add(cur!!)
        return newList
    }
}
