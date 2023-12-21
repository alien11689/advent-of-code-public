package dpr.aoc2023

import dpr.commons.Point2D
import dpr.commons.Util

object Day21 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/21/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/21/test1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val garden = mutableMapOf<Point2D, Char>()
        var start = Point2D(0, 0)
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                val cur = Point2D(x, y)
                when (c) {
                    'S' -> {
                        garden[cur] = '.'
                        start = cur
                    }

                    else -> {
                        garden[cur] = c
                    }
                }
            }
        }
        var i = 0
        var available = setOf(start)
        while (i < 64) {
            println("Checking $i")
            available = available.flatMap { it.neighboursCross() }.toSet().filter { garden[it] == '.' }.toSet()
            ++i
        }
        return available.size
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

