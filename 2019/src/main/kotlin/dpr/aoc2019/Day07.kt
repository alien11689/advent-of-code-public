package dpr.aoc2019

import dpr.aoc2019.intcode.IntCodeComputer
import dpr.aoc2019.intcode.IntCodeComputerState
import dpr.commons.Util

object Day07 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/07/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: String): Long {
        val phasesSet = permutations((0L..4L).toSet())

        return phasesSet.maxOf { phases ->
            var signal = 0L
            phases.forEach { phase ->
                val state = IntCodeComputerState.init(input)
                state.input.offer(phase)
                state.input.offer(signal)
                IntCodeComputer.program(state)
                signal = state.output.last()
            }
            signal
        }

    }

    private fun <T> permutations(input: Set<T>): Set<List<T>> {
        return if (input.size == 1) {
            setOf(input.toList())
        } else {
            input.flatMap { cur ->
                permutations(input - cur).map { it + cur }
            }.toSet()
        }
    }

    @JvmStatic
    fun part2(input: String): Long {
        val phasesSet = permutations((5L..9L).toSet())

        return phasesSet.maxOf { phases ->
            val states = mutableMapOf<Int, IntCodeComputerState>()
            for (i in phases.indices) {
                val state = IntCodeComputerState.init(input)
                state.input.offer(phases[i])
                states[i] = state
            }
            states[0]!!.input.offer(0L)
            var cur = 0
            while (!states.values.all { it.ended }) {
                val nextCur = (cur + 1) % phases.size
                IntCodeComputer.program(states[cur]!!, states[nextCur]!!.input)
                cur = nextCur
            }
            states[0]!!.input.poll()
        }
    }
}
