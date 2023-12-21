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
        val (garden, start) = readGarden(lines)
        var i = 0
        var prev = emptySet<Point2D>()
        var newlyAdded = setOf(start)
        var available = setOf(start)
        while (i < 64) {
            println("Checking $i")
            newlyAdded = newlyAdded.flatMap { it.neighboursCross() }.filter { garden[it] == '.' && it !in prev }.toSet()
            val prevBck = prev
            prev = available
            available = prevBck + newlyAdded
            ++i
        }
        return available.size
    }

    private fun readGarden(lines: List<String>): Pair<MutableMap<Point2D, Char>, Point2D> {
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
        return Pair(garden, start)
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

