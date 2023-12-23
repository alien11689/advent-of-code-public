package dpr.aoc2023

import dpr.commons.Dir
import dpr.commons.Point2D
import dpr.commons.Util
import java.util.Stack

object Day23 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/23/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/23/test1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    data class Route(val point: Point2D, val dir: Dir, val length: Int)

    private fun part1(lines: List<String>): Any {
        val board = mutableMapOf<Point2D, Char>()
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                board[Point2D(x, y)] = c
            }
        }
        val start = Point2D(1, 0)
        val end = Point2D(lines[0].length - 2, lines.size - 1)
        val routes = Stack<Route>()
        routes.push(Route(start, Dir.S, 0))
        val finishedRoutes = mutableSetOf<Int>()
        while (routes.isNotEmpty()) {
            val cur = routes.pop()
//            println("Checking $cur")
            var l = cur.length
            var point = cur.point
            var dir = cur.dir
            while (true) {
                val possibleNextSteps = listOf(dir, dir.turnLeft(), dir.turnRight()).mapNotNull { nextDir ->
                    val next = point.move(nextDir)
                    val tile = board[next]
                    when (tile) {
                        '#' -> null
                        '.' -> nextDir to next
                        '>' -> if (nextDir == Dir.E) nextDir to next else null
                        '<' -> if (nextDir == Dir.W) nextDir to next else null
                        '^' -> if (nextDir == Dir.N) nextDir to next else null
                        'v' -> if (nextDir == Dir.S) nextDir to next else null
                        else -> throw RuntimeException("Hitting $next")
                    }
                }
                l++
                if (possibleNextSteps.size > 1) {
                    possibleNextSteps.forEach { (nextDir, next) ->
                        routes.push(Route(next, nextDir, l))
                    }
                    break
                } else {
                    val (nextDir, next) = possibleNextSteps.single()
                    dir = nextDir
                    point = next
                    if (point == end) {
                        finishedRoutes.add(l)
                        break
                    }
                }
            }
        }
        return finishedRoutes.max()
    }

    private fun part2(lines: List<String>): Any {
        return "TODO"
    }
}

