package pl.touk.dpr.aoc2019

import pl.touk.dpr.aoc2019.intcode.IntCodeComputer
import pl.touk.dpr.aoc2019.intcode.IntCodeComputer.program
import pl.touk.dpr.aoc2019.intcode.IntCodeComputerState
import java.util.LinkedList

object Day17 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/17/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val v = IntCodeComputer.parseInput(input)
        val output = LinkedList<Long>()
        val inputQ = LinkedList<Long>()
        val state = IntCodeComputerState(v, input = inputQ)
        program(state, output)

        var i = 0L
        var j = 0L

        val m = mutableMapOf<Pair<Long, Long>, Long>()
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
        for (j in m.keys.map { it.second }.minOrNull()!!..m.keys.map { it.second }.maxOrNull()!!) {
            for (i in m.keys.map { it.first }.minOrNull()!!..m.keys.map { it.first }.maxOrNull()!!) {
                if (m[i to j] == 35L && m[i + 1 to j] == 35L && m[i - 1 to j] == 35L && m[i to j + 1] == 35L && m[i to j - 1] == 35L) {
                    sum += i * j
                }
            }
        }
        return sum
    }

    private fun part2(input: String): Any {
        val v = IntCodeComputer.parseInput(input)
        val output = LinkedList<Long>()
        val inputQ = LinkedList<Long>()
        val state = IntCodeComputerState(v, input = inputQ)
        state.v[0L] = 2L
        program(state, output)

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
        val y = 79L

        //  R6L8R8R6L8R8R4R6R6R4R4L8R6L10L10R4R6R6R4R4L8R6L10L10R4R6R6R4R4L8R6LL10L10R6L8R8L8R6L10L10
        // A => L8R6L10L10
        //  R6L8R8R6L8R8R4R6R6R4R4AR4R6R6R4R4AR4R6R6R4R4AR6L8R8A
        // B => R6L8R8
        //  BBR4R6R6R4R4AR4R6R6R4R4AR4R6R6R4R4ABA
        // C => R4R6R6R4R4A
        //  BBCACACABA

        inputQ.offer(B)
        inputQ.offer(coma)
        inputQ.offer(B)
        inputQ.offer(coma)
        inputQ.offer(C)
        inputQ.offer(coma)
        inputQ.offer(A)
        inputQ.offer(coma)
        inputQ.offer(C)
        inputQ.offer(coma)
        inputQ.offer(A)
        inputQ.offer(coma)
        inputQ.offer(C)
        inputQ.offer(coma)
        inputQ.offer(A)
        inputQ.offer(coma)
        inputQ.offer(B)
        inputQ.offer(coma)
        inputQ.offer(A)
        inputQ.offer(nl)

//A
        inputQ.offer(L)
        inputQ.offer(coma)
        inputQ.offer(_8)
        inputQ.offer(coma)
        inputQ.offer(R)
        inputQ.offer(coma)
        inputQ.offer(_6)
        inputQ.offer(coma)
        inputQ.offer(L)
        inputQ.offer(coma)
        inputQ.offer(_1)
        inputQ.offer(_0)
        inputQ.offer(coma)
        inputQ.offer(L)
        inputQ.offer(coma)
        inputQ.offer(_1)
        inputQ.offer(_0)
        inputQ.offer(nl)

// B
        inputQ.offer(R)
        inputQ.offer(coma)
        inputQ.offer(_6)
        inputQ.offer(coma)
        inputQ.offer(L)
        inputQ.offer(coma)
        inputQ.offer(_8)
        inputQ.offer(coma)
        inputQ.offer(R)
        inputQ.offer(coma)
        inputQ.offer(_8)
        inputQ.offer(nl)

//C
        inputQ.offer(R)
        inputQ.offer(coma)
        inputQ.offer(_4)
        inputQ.offer(coma)
        inputQ.offer(R)
        inputQ.offer(coma)
        inputQ.offer(_6)
        inputQ.offer(coma)
        inputQ.offer(R)
        inputQ.offer(coma)
        inputQ.offer(_6)
        inputQ.offer(coma)
        inputQ.offer(R)
        inputQ.offer(coma)
        inputQ.offer(_4)
        inputQ.offer(coma)
        inputQ.offer(R)
        inputQ.offer(coma)
        inputQ.offer(_4)
        inputQ.offer(nl)

//mode
        inputQ.offer(n)
        inputQ.offer(nl)

        program(state, output)

        return output.last
    }
}
