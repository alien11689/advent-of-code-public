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

    @JvmStatic
    @JvmOverloads
    fun part1(input: List<String>, steps: Int = 100): Int {
        var points = input.map { line ->
            line.map { it == '#' }
        }
        val size = input.size
        repeat(steps) {
            points = points.mapIndexed { y, line ->
                line.mapIndexed { x, on ->
                    val onNeighbours = neighbours(x, y, size).count { points[it.y][it.x] }
                    on && onNeighbours in setOf(2, 3) || !on && onNeighbours == 3
                }
            }
        }
        return points.flatten().count { it }
    }

    private fun neighbours(x: Int, y: Int, size: Int): Set<Point2D> =
        Point2D(x, y).adjacentPoints()
            .filter { it.x in 0..<size && it.y in 0..<size }
            .toSet()

    @JvmStatic
    @JvmOverloads
    fun part2(input: List<String>, steps: Int = 100): Int {
        var points = input.mapIndexed { y, line ->
            line.mapIndexed { x, c -> c == '#' || x in setOf(0, 99) && y in setOf(0, 99) }
        }
        val size = input.size
        repeat(steps) {
            points = points.mapIndexed { y, line ->
                line.mapIndexed { x, on ->
                    val onNeighbours = neighbours(x, y, size).count { points[it.y][it.x] }
                    x in setOf(0, size - 1) && y in setOf(0, size - 1) || on && onNeighbours in setOf(
                        2,
                        3
                    ) || !on && onNeighbours == 3
                }
            }
        }
        return points.flatten().count { it }
    }
}
