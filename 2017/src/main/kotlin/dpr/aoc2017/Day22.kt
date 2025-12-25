package dpr.aoc2017

import dpr.commons.Dir
import dpr.commons.Point2D
import dpr.commons.Util

object Day22 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/22/input.txt")
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(lines: List<String>, iterations: Int = 10_000): Int {
        val grid = mutableMapOf<Point2D, Boolean>()
        val cur = Point2D(lines[0].length / 2, lines.size / 2)

        var j = 0
        lines.forEach { row ->
            var i = 0
            row.forEach { cell ->
                if (cell == '#') {
                    grid[Point2D(i, j)] = true
                }
                ++i
            }
            ++j

        }

        val virus = Virus(cur)
        repeat(iterations) {
            virus.burst(grid)
        }

        return virus.infected

    }

    @JvmStatic
    fun part2(lines: List<String>, iterations: Int = 10_000_000): Int {
        val grid = mutableMapOf<Point2D, Status>()
        val cur = Point2D(lines[0].length / 2, lines.size / 2)

        var j = 0
        lines.forEach { row ->
            var i = 0
            row.forEach { cell ->
                if (cell == '#') {
                    grid[Point2D(i, j)] = Status.Infected
                }
                ++i
            }
            ++j

        }

        val virus = Virus(cur)
        repeat(iterations) {
            virus.burst2(grid)
        }

        return virus.infected
    }

    enum class Status {
        Clean,
        Weakened,
        Infected,
        Flagged;

        fun next(): Status {
            return when (this) {
                Clean -> Weakened
                Weakened -> Infected
                Infected -> Flagged
                Flagged -> Clean
            }
        }

    }

    data class Virus(var p: Point2D, var dir: Dir = Dir.N, var infected: Int = 0) {
        fun burst(grid: MutableMap<Point2D, Boolean>) {
            val value = grid[p] ?: false
            dir = if (value) dir.turnRight() else dir.turnLeft()
            grid[p] = !value
            if (grid[p]!!) {
                infected++
            }
            p = p.move(dir)
        }

        fun burst2(grid: MutableMap<Point2D, Status>) {
            val value = grid[p] ?: Status.Clean
            when (value) {
                Status.Clean -> dir = dir.turnLeft()
                Status.Weakened -> {
                }

                Status.Infected -> dir = dir.turnRight()
                Status.Flagged -> dir = dir.opposite()
            }
            grid[p] = value.next()
            if (grid[p] == Status.Infected) {
                infected++
            }
            p = p.move(dir)
        }
    }
}
