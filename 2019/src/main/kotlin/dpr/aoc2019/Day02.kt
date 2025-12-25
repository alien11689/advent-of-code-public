package dpr.aoc2019

import dpr.aoc2019.intcode.IntCodeComputer
import dpr.aoc2019.intcode.IntCodeComputerState
import dpr.commons.Util

object Day02 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/02/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: String): Long {
        val state = IntCodeComputerState.init(input)
        state.v[1L] = 12L
        state.v[2L] = 2L
        IntCodeComputer.program(state)
        return state.v[0L]!!
    }

    @JvmStatic
    fun part2(input: String): Int {
        for (i in 0 until 100) {
            for (j in 0 until 100) {
                val state = IntCodeComputerState.init(input)
                state.v[1L] = i.toLong()
                state.v[2L] = j.toLong()
                IntCodeComputer.program(state)
                val res = state.v[0L] ?: 0L
                if (res == 19690720L) {
                    return i * 100 + j
                }
            }
        }
        throw RuntimeException()
    }
}
