package dpr.aoc2019

import dpr.aoc2019.intcode.IntCodeComputer.program
import dpr.aoc2019.intcode.IntCodeComputerState
import java.util.LinkedList

object Day13 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/13/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val state = IntCodeComputerState.init(input)
        val output = state.output

        val panel = mutableMapOf<Pair<Long, Long>, Long>()
        program(state, output)
        var countB = 0
        while (output.size > 0) {
            val x = output.poll()
            val y = output.poll()
            val t = output.poll()
            panel[x to y] = t
            if (t == 2L) {
                ++countB
            }
        }
        return countB
    }

    private fun part2(input: String): Any {
        val state = IntCodeComputerState.init(input)
        val output = state.output
        val inputQ = state.input

        state.v[0L] = 2L
        program(state)

        val panel = mutableMapOf<Pair<Long, Long>, Long>()
        createBoard(panel, output)

        inputQ.offer(1)

        fun findB(panel: MutableMap<Pair<Long, Long>, Long>): Pair<Long, Long> {
            return panel.filter { it.value == 4L }.entries.first().key
        }

        fun findH(panel: MutableMap<Pair<Long, Long>, Long>): Pair<Long, Long> {
            return panel.filter { it.value == 3L }.entries.first().key
        }

        var i = 0
        while (!state.ended) {
            ++i
            //println('===============================================')
            program(state, output)
            createBoard(panel, output)
            //printBoard(panel)
            val curB = findB(panel)
            val curH = findH(panel)
            //println("Ball is on " + curB)
            //println("H is on " + curH)
            if (curB.first == curH.first) {
                inputQ.offer(0)
            } else if (curB.first < curH.first) {
                inputQ.offer(-1)
            } else {
                inputQ.offer(1)
            }
        }
        return panel[-1L to 0L]!!
    }

    private fun createBoard(panel: MutableMap<Pair<Long, Long>, Long>, output: LinkedList<Long>) {
        while (output.size > 0) {
            val x = output.poll()
            val y = output.poll()
            val t = output.poll()
            panel[x to y] = t
        }
    }

//    fun printBoard(panel: MutableMap<Pair<Long, Long>, Long>) {
//        for (i in (panel.keys.minByOrNull { it.second }!!.second..panel.keys.maxByOrNull { it.second }!!.second)) {
//            for (j in (panel.keys.minByOrNull { it.first }!!.first..panel.keys.maxByOrNull { it.first }!!.first)) {
//                if (j == -1L) {
//                    if (i == 0L) {
//                        println("Score: " + panel[j to i])
//                    }
//                    continue
//                }
//                val v = panel[j to i]?.toInt()
//                if (v == null) {
//                    continue
//                }
//                if (v == 0) {
//                    print('.')
//                } else if (v == 1) {
//                    print('#')
//                } else if (v == 2) {
//                    print('B')
//                } else if (v == 3) {
//                    print('H')
//                } else if (v == 4) {
//                    print('O')
//                }
//            }
//            println()
//        }
//    }
}
