package dpr.aoc2016

import dpr.commons.Dir
import dpr.commons.Point2D
import dpr.commons.Util

object Day01 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/01/input.txt").trim().split(Regex("[ ,]+"))
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val zero = Point2D(0, 0)
        return input.fold(Position(zero)) { acc, c -> acc.go(c) }.point.manhattan(zero)
    }

    private fun part2(input: List<String>): Any {
        val mem = mutableSetOf<Point2D>()
        val zero = Point2D(0, 0)
        var cur = Position(zero)
        mem.add(cur.point)
        input.forEach {
            val turn = it.first()
            var step = it.drop(1).toInt() - 1
            cur = cur.go(turn + "1")
            if (cur.point in mem) {
                return cur.point.manhattan(zero)
            }
            mem.add(cur.point)
            while (step > 0) {
                cur = cur.go("C1")
                if (cur.point in mem) {
                    return cur.point.manhattan(zero)
                }
                mem.add(cur.point)
                --step
            }
        }
        throw RuntimeException()
    }

    data class Position(val point: Point2D, val dir: Dir = Dir.N) {
        fun go(input: String): Position {
            val nextDir = when (input.first()) {
                'L' -> dir.turnLeft()
                'R' -> dir.turnRight()
                else -> dir
            }
            val step = input.drop(1).toInt()
            return Position(dir = nextDir, point = point.move(nextDir, step))
        }
    }
}

