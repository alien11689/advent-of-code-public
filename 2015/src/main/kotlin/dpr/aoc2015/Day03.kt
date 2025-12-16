package dpr.aoc2015

import dpr.commons.Point2D
import dpr.commons.Util

object Day03 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/03/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: String): Any {
        return input.fold(listOf(Point2D(0, 0))) { acc, s ->
            acc + nextPos(s, acc.last())
        }.toSet().count()
    }

    @JvmStatic
    fun part2(input: String): Any {
        return IntRange(0, 1).flatMap { santa ->
            input.foldIndexed(listOf(Point2D(0, 0))) { i, acc, s ->
                if (i % 2 == santa) {
                    acc
                } else {
                    acc + nextPos(s, acc.last())
                }
            }
        }.toSet().size
    }

    private fun nextPos(s: Char, cur: Point2D) = when (s) {
        '>' -> cur.right()
        '<' -> cur.left()
        '^' -> cur.up()
        'v' -> cur.down()
        else -> throw RuntimeException()
    }
}
