package dpr.aoc2015

import dpr.commons.Point2D
import dpr.commons.Util

object Day18 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/18/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        var points = input.map { line ->
            line.map { it == '#' }
        }
        repeat(100) {
            points = points.mapIndexed { y, line ->
                line.mapIndexed { x, on ->
                    val onNeighbours = neighbours(x, y).count { points[it.y][it.x] }
                    on && onNeighbours in setOf(2, 3) || !on && onNeighbours == 3
                }
            }
        }
        return points.flatten().count { it }
    }

    private fun neighbours(x: Int, y: Int): Set<Point2D> =
        Point2D(x, y).adjacentPoints()
            .filter { it.x in 0..99 && it.y in 0..99 }
            .toSet()

    private fun part2(input: List<String>): Any {
        var points = input.mapIndexed { y, line ->
            line.mapIndexed { x, c -> c == '#' || x in setOf(0, 99) && y in setOf(0, 99) }
        }
        repeat(100) {
            points = points.mapIndexed { y, line ->
                line.mapIndexed { x, on ->
                    val onNeighbours = neighbours(x, y).count { points[it.y][it.x] }
                    x in setOf(0, 99) && y in setOf(0, 99) || on && onNeighbours in setOf(2, 3) || !on && onNeighbours == 3
                }
            }
        }
        return points.flatten().count { it }
    }
}
