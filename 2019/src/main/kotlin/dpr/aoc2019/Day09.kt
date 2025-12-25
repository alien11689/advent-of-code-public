package dpr.aoc2019

import dpr.aoc2019.intcode.IntCodeComputer
import dpr.aoc2019.intcode.IntCodeComputerState
import dpr.commons.Util

object Day09 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/09/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: String): Long {
        val state = IntCodeComputerState.init(input)
        state.input.offer(1L)
        IntCodeComputer.program(state)
        return state.output.last()
    }

    @JvmStatic
    fun part2(input: String): Long {
        val state = IntCodeComputerState.init(input)
        state.input.offer(2L)
        IntCodeComputer.program(state)
        return state.output.last()
    }
}
