package pl.touk.dpr.aoc2019

import pl.touk.dpr.aoc2019.intcode.IntCodeComputer
import pl.touk.dpr.aoc2019.intcode.IntCodeComputerState

object Day09 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/09/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val state = IntCodeComputerState.init(input)
        state.input.offer(1L)
        IntCodeComputer.program(state)
        return state.output.last
    }

    private fun part2(input: String): Any {
        val state = IntCodeComputerState.init(input)
        state.input.offer(2L)
        IntCodeComputer.program(state)
        return state.output.last
    }
}
