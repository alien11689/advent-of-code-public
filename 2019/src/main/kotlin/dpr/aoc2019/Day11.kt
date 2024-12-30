package dpr.aoc2019

import dpr.aoc2019.intcode.IntCodeComputer
import dpr.aoc2019.intcode.IntCodeComputerState
import dpr.commons.Dir
import dpr.commons.Point2D
import dpr.commons.Util

object Day11 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/11/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val state = IntCodeComputerState.init(input)

        val panel = createPanel(state)
        return panel.size
    }

    private fun createPanel(state: IntCodeComputerState, initValue: Long? = null): MutableMap<Point2D, Long> {
        val inputQ = state.input
        val output = state.output
        val panel = mutableMapOf<Point2D, Long>()
        var curPos = Point2D(0, 0)
        if (initValue != null) {
            panel[curPos] = initValue
        }
        var dir = Dir.N
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

    private fun part2(input: String): String {
        val state = IntCodeComputerState.init(input)

        val panel = createPanel(state, 1L)
        val whites = panel.filter { it.value == 1L }.keys
//        println(whites)
        val maxY = whites.maxByOrNull { it.y }!!.y
        val minY = whites.minByOrNull { it.y }!!.y
        val minX = whites.minByOrNull { it.x }!!.x
        val maxX = whites.maxByOrNull { it.x }!!.x
        val sb = StringBuilder()
        for (i in minY..maxY) {
            for (j in (minX..maxX)) {
//                println("${Pair(j, i)} in whites: ${Pair(j, i) in whites}")
                sb.append(if (Point2D(j, i) in whites) 'X' else ' ')
            }
            if (i < maxY) {
                sb.append('\n')
            }
        }
        return sb.toString()
    }

    fun Dir.move(pos: Point2D, v: Long): Pair<Dir, Point2D> {
        val dir = if (v == 0L) turnLeft() else turnRight()
        return dir to pos.move(dir)
    }
}
