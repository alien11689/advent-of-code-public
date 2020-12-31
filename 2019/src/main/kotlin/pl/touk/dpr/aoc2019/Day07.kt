package pl.touk.dpr.aoc2019

import pl.touk.dpr.aoc2019.intcode.IntCodeComputer
import pl.touk.dpr.aoc2019.intcode.IntCodeComputerState
import java.util.LinkedList

object Day07 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getFileContent("/07/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val phasesSet = permutations((0L..4L).toSet())

        return phasesSet.map { phases ->
            var signal = 0L
            phases.forEach { phase ->
                val v = IntCodeComputer.parseInput(input)
                val output = LinkedList<Long>()
                val inputQ = LinkedList<Long>()
                inputQ.offer(phase)
                inputQ.offer(signal)
                IntCodeComputer.program(IntCodeComputerState(v, input = inputQ), output)
                signal = output.last
            }
            signal
        }.maxOrNull()!!

    }

    private fun <T> permutations(input: Set<T>): Set<List<T>> {
        if (input.size == 1) {
            return setOf(input.toList())
        } else {
            return input.flatMap { cur ->
                permutations(input - cur).map { it + cur }
            }.toSet()
        }
    }

    private fun part2(input: String): Any {
        val phasesSet = permutations((5L..9L).toSet())

        return phasesSet.map { phases ->
            val states = mutableMapOf<Int, IntCodeComputerState>()
            for (i in phases.indices) {
                val v = IntCodeComputer.parseInput(input)
                val inputQ = LinkedList<Long>()
                inputQ.offer(phases[i])
                states[i] = IntCodeComputerState(v, input = inputQ)
            }
            states[0]!!.input.offer(0L)
            var cur = 0
            while (!states.values.all { it.ended }) {
                val nextCur = (cur + 1) % phases.size
                IntCodeComputer.program(states[cur]!!, states[nextCur]!!.input)
                cur = nextCur
            }
            states[0]!!.input.poll()
        }.maxOrNull()!!
    }
}