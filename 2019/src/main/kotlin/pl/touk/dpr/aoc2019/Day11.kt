package pl.touk.dpr.aoc2019

import pl.touk.dpr.aoc2019.intcode.IntCodeComputer
import pl.touk.dpr.aoc2019.intcode.IntCodeComputerState

object Day11 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/11/input.txt").trim()
        println(part1(input))
        part2(input)
    }

    private fun part1(input: String): Any {
        val state = IntCodeComputerState.init(input)

        val panel = createPanel(state)
        return panel.size
    }

    private fun createPanel(state: IntCodeComputerState, initValue: Long? = null): MutableMap<Pair<Int, Int>, Long> {
        val inputQ = state.input
        val output = state.output
        val panel = mutableMapOf<Pair<Int, Int>, Long>()
        var curPos = Pair(0, 0)
        if (initValue != null) {
            panel[curPos] = initValue
        }
        var dir = Dir.UP
        while (!state.ended) {
            val color = panel[curPos] ?: 0L
            inputQ.offer(color)
            IntCodeComputer.program(state, output)
            panel[curPos] = output.poll()
            val res = dir.move(curPos, output.poll())
            dir = res.first
            curPos = res.second
        }
        return panel
    }

    private fun part2(input: String) {
        val state = IntCodeComputerState.init(input)

        val panel = createPanel(state, 1L)
        val whites = panel.filter { it.value == 1L }.keys
//        println(whites)
        val maxY = whites.maxByOrNull { it.second }!!.second
        val minY = whites.minByOrNull { it.second }!!.second
        val minX = whites.minByOrNull { it.first }!!.first
        val maxX = whites.maxByOrNull { it.first }!!.first
        for (i in maxY downTo minY) {
            for (j in (minX..maxX)) {
//                println("${Pair(j, i)} in whites: ${Pair(j, i) in whites}")
                print(if (Pair(j, i) in whites) 'X' else ' ')
            }
            println()
        }
    }

    enum class Dir {
        UP, LEFT, DOWN, RIGHT;

        fun move(pos: Pair<Int, Int>, v: Long): Pair<Dir, Pair<Int, Int>> {
            return when (this) {
                UP -> if (v == 0L) Pair(LEFT, Pair(pos.first - 1, pos.second)) else Pair(
                        RIGHT,
                        Pair(pos.first + 1, pos.second)
                )

                DOWN -> if (v == 0L) Pair(RIGHT, Pair(pos.first + 1, pos.second)) else Pair(
                        LEFT,
                        Pair(pos.first - 1, pos.second)
                )

                LEFT -> if (v == 0L) Pair(DOWN, Pair(pos.first, pos.second - 1)) else Pair(
                        UP,
                        Pair(pos.first, pos.second + 1)
                )

                RIGHT -> if (v == 0L) Pair(UP, Pair(pos.first, pos.second + 1)) else Pair(
                        DOWN,
                        Pair(pos.first, pos.second - 1)
                )
            }
        }
    }
}
