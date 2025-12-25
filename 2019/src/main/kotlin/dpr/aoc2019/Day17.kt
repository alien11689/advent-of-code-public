package dpr.aoc2019

import dpr.aoc2019.intcode.IntCodeComputer.program
import dpr.aoc2019.intcode.IntCodeComputerState
import dpr.commons.Util

object Day17 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/17/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: String): Long {
        val state = IntCodeComputerState.init(input)
        program(state)

        var i = 0L
        var j = 0L

        val m = mutableMapOf<Pair<Long, Long>, Long>()
        val output = state.output
        while (output.isNotEmpty()) {
            val cur = output.poll()
            when (cur.toInt()) {
                35 -> {
//                    print('#')
                    m[i++ to j] = cur
                }

                46 -> {
//                    print('.')
                    m[i++ to j] = cur
                }

                94 -> {
//                    print('^')
                    m[i++ to j] = cur
                }

                60 -> {
//                    print('<')
                    m[i++ to j] = cur
                }

                62 -> {
//                    print('>')
                    m[i++ to j] = cur
                }

                118 -> {
//                    print('v')
                    m[i++ to j] = cur
                }

                10 -> {
//                    println()
                    j++
                    i = 0
                }

                else -> throw RuntimeException("$cur")
            }
        }

        var sum = 0L
        for (jj in m.keys.minOf { it.second }..m.keys.maxOf { it.second }) {
            for (ii in m.keys.minOf { it.first }..m.keys.maxOf { it.first }) {
                if (m[ii to jj] == 35L && m[ii + 1 to jj] == 35L && m[ii - 1 to jj] == 35L && m[ii to jj + 1] == 35L && m[ii to jj - 1] == 35L) {
                    sum += ii * jj
                }
            }
        }
        return sum
    }

    @JvmStatic
    fun part2(input: String): Long {
        val state = IntCodeComputerState.init(input)
        val inputQ = state.input
        state.v[0L] = 2L
        program(state)

        val R = 82L
        val L = 76L
        val A = 65L
        val B = 66L
        val C = 67L
        val coma = 44L
        val _8 = 56L
        val _4 = 52L
        val _6 = 54L
        val _1 = 49L
        val _0 = 48L
        val nl = 10L
        val n = 110L
//        val y = 79L

        //  R6L8R8R6L8R8R4R6R6R4R4L8R6L10L10R4R6R6R4R4L8R6L10L10R4R6R6R4R4L8R6LL10L10R6L8R8L8R6L10L10
        // A => L8R6L10L10
        //  R6L8R8R6L8R8R4R6R6R4R4AR4R6R6R4R4AR4R6R6R4R4AR6L8R8A
        // B => R6L8R8
        //  BBR4R6R6R4R4AR4R6R6R4R4AR4R6R6R4R4ABA
        // C => R4R6R6R4R4A
        //  BBCACACABA

        listOf(
            B,
            coma,
            B,
            coma,
            C,
            coma,
            A,
            coma,
            C,
            coma,
            A,
            coma,
            C,
            coma,
            A,
            coma,
            B,
            coma,
            A,
            nl,
        ).forEach {
            inputQ.offer(it)
        }

        //A
        listOf(
            L,
            coma,
            _8,
            coma,
            R,
            coma,
            _6,
            coma,
            L,
            coma,
            _1,
            _0,
            coma,
            L,
            coma,
            _1,
            _0,
            nl,
        ).forEach {
            inputQ.offer(it)
        }

        // B
        listOf(
            R,
            coma,
            _6,
            coma,
            L,
            coma,
            _8,
            coma,
            R,
            coma,
            _8,
            nl,
        ).forEach {
            inputQ.offer(it)
        }

        //C
        listOf(
            R,
            coma,
            _4,
            coma,
            R,
            coma,
            _6,
            coma,
            R,
            coma,
            _6,
            coma,
            R,
            coma,
            _4,
            coma,
            R,
            coma,
            _4,
            nl,
        ).forEach {
            inputQ.offer(it)
        }

//mode
        listOf(
            n,
            nl,
        ).forEach {
            inputQ.offer(it)
        }

        program(state)

        return state.output.last()
    }
}
