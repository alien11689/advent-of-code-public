package pl.touk.dpr.aoc2019

import pl.touk.dpr.aoc2019.intcode.IntCodeComputer
import pl.touk.dpr.aoc2019.intcode.IntCodeComputer.instruction
import pl.touk.dpr.aoc2019.intcode.IntCodeComputer.program
import pl.touk.dpr.aoc2019.intcode.IntCodeComputerState
import java.util.LinkedList

object Day21 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getFileContent("/21/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val v = IntCodeComputer.parseInput(input)
        val output = LinkedList<Long>()
        val inputQ = LinkedList<Long>()
        val state = IntCodeComputerState(v, input = inputQ)

        listOf(
            "NOT C J",
            "AND D J",
            "NOT A T",
            "OR T J"
//        'NOT B T',
//        'AND T J',
//        'NOT C T',
//        'AND T J',
//        'AND D J',
        ).forEach { instruction(inputQ, it) }

        instruction(inputQ, "WALK")

        program(state, output)

        var last: Long = -1L
        while (!output.isEmpty()) {
            val cur = output.poll()
            last = cur
            when (cur) {
                10L -> {
//                    println()
                }
                else -> {
//                    print(cur.toChar())
                    last = cur
                }
            }
        }
        return last
    }

    private fun part2(input: String): Any {
        val v = IntCodeComputer.parseInput(input)
        val output = LinkedList<Long>()
        val inputQ = LinkedList<Long>()
        val state = IntCodeComputerState(v, input = inputQ)

        listOf(
            "fttttttttt",
            "tftttttttf",
            "#.##...##t",
            "##.#.#..#f",
            "#.#.#..##f",
            ".#.#..###t",
            "#..#.####t",
        ).map { it.map { it == 't' || it == '#' } }
            .forEach { check(it) }

        listOf(
            //E || H -> J
            "OR E J",
            "OR H J",

            // D && !C -> T
            "NOT C T",
            "AND D T",

            // D && !C && (E || H) -> J
            "AND T J",

            //!A || D && !C && (E || H) -> J
            "NOT A T",
            "OR T J",

            "NOT E T",
            "NOT T T",
            "OR B T",
            "NOT T T",

            "OR T J",

            "RUN"
        ).forEach { instruction(inputQ, it) }

        program(state, output)

        var last: Long = -1L
        while (!output.isEmpty()) {
            val cur = output.poll()
            last = cur
            when (cur) {
                10L -> {
//                    println()
                }
                else -> {
//                    print(cur.toChar())
                    last = cur
                }
            }
        }
        return last
    }

    private fun check(input: List<Boolean>) {
        val A = input[0]
        val B = input[1]
        val C = input[2]
        val D = input[3]
        val E = input[4]
        val F = input[5]
        val G = input[6]
        val H = input[7]
        val I = input[8]
        val EXPECTED = input[9]
//    if ((!A || ((D && !C) && (E || H) || (!B && !E))) != EXPECTED) { //działa
        if ((!A || ((D && !C) && (E || H) || !(B || !(!E)))) != EXPECTED) {
            throw RuntimeException(
                "FAIL for ${
                    input.take(9).map { if (it) '#' else '.' }.joinToString("")
                } expected ${if (EXPECTED) "JUMP" else "NOT JUMP"}"
            )
        }
    }
}