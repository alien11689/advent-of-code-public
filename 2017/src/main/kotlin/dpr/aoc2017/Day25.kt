package dpr.aoc2017

import dpr.commons.Pair
import dpr.commons.Util

object Day25 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        println(part1())
    }

    @JvmStatic
    fun part1(turingMachine: TuringMachine = TuringMachine()): Int {
        var state = turingMachine.initState

        val steps = turingMachine.steps

        val tape = Tape()

        repeat(steps) {
            val cur = Pair(state, tape.read())
            val (write, move, next) = turingMachine.transistions[cur]!!
            tape.write(write)
            tape.move(move)
            state = next
        }

        return tape.value()

    }

    class Tape {
        var cur = 0
        var values = mutableMapOf<Int, Int>()

        fun read(): Int {
            return values[cur] ?: 0
        }

        fun write(v: Int) {
            values[cur] = v
        }

        fun value(): Int {
            return values.values.sum()
        }

        fun move(move: Int) {
            cur += move
        }
    }

    data class TuringMachine(
        val initState: Char = 'A',
        val steps: Int = 12656374,
        val transistions: Map<Pair<Char, Int>, Transition> = mapOf(
            Pair('A', 0) to Transition(1, 1, 'B'),
            Pair('A', 1) to Transition(0, -1, 'C'),
            Pair('B', 0) to Transition(1, -1, 'A'),
            Pair('B', 1) to Transition(1, -1, 'D'),
            Pair('C', 0) to Transition(1, 1, 'D'),
            Pair('C', 1) to Transition(0, 1, 'C'),
            Pair('D', 0) to Transition(0, -1, 'B'),
            Pair('D', 1) to Transition(0, 1, 'E'),
            Pair('E', 0) to Transition(1, 1, 'C'),
            Pair('E', 1) to Transition(1, -1, 'F'),
            Pair('F', 0) to Transition(1, -1, 'E'),
            Pair('F', 1) to Transition(1, 1, 'A'),
        )
    )

    data class Transition(val write: Int, val move: Int, val next: Char)
}
