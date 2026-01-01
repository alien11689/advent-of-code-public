package dpr.aoc2020

import dpr.commons.Util

object Day15 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = "1,2,16,19,18,0"
        part1And2(input).forEach(::println)
    }

    @JvmStatic
    fun part1And2(input: String): List<Int> {
        val mem = mutableMapOf<Int, Int>()
        var current = 0
        input.split(",").mapIndexed { index, s ->
            mem[s.toInt()] = index + 1
            current = s.toInt()
        }
        var round = mem.size
        val res = mutableListOf<Int>()
        while (round < 30000000 + 1) {
            if (round == 2020 || round == 30000000) {
//                println("round: $round, current: $current")
                res.add(current)
            }
            if (current in mem) {
                val newCurrent = round - mem[current]!!
                mem[current] = round
                current = newCurrent
            } else {
                mem[current] = round
                current = 0
            }
            round++
        }
        return res
    }
}

