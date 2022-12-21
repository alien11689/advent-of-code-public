package pl.touk.dpr.aoc2019

import pl.touk.dpr.aoc2019.intcode.IntCodeComputer
import pl.touk.dpr.aoc2019.intcode.IntCodeComputerState
import java.util.LinkedList

object Day09 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/09/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val v = IntCodeComputer.parseInput(input)
        val output = LinkedList<Long>()
        val inputQ = LinkedList<Long>()
        inputQ.offer(1L)
        IntCodeComputer.program(IntCodeComputerState(v, input = inputQ), output)
        return output.last
    }

    private fun part2(input: String): Any {
        val v = IntCodeComputer.parseInput(input)
        val output = LinkedList<Long>()
        val inputQ = LinkedList<Long>()
        inputQ.offer(2L)
        IntCodeComputer.program(IntCodeComputerState(v, input = inputQ), output)
        return output.last
    }
}
