package dpr.aoc2023

import dpr.commons.Dir
import dpr.commons.Point2D
import dpr.commons.Util
import java.util.Stack

object Day16 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/16/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/16/test1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    data class Current(val dir: Dir, val pos: Point2D) {
        fun move(): Point2D = when (dir) {
            Dir.N -> pos.up()
            Dir.W -> pos.left()
            Dir.S -> pos.down()
            Dir.E -> pos.right()
        }
    }

    private fun part1(lines: List<String>): Any {
        val mirrors = readMirrors(lines)
        val maxX = lines[0].length - 1
        val maxY = lines.size - 1
        val start = Current(Dir.E, Point2D(-1, 0))
        val energized = getEnergized(start, maxX, maxY, mirrors)
        return energized.size
    }

    private fun readMirrors(lines: List<String>): Map<Point2D, Char> {
        return Util.readBoard(lines).filter { it.value != '.' }
    }

    private fun getEnergized(
        start: Current,
        maxX: Int,
        maxY: Int,
        mirrors: Map<Point2D, Char>
    ): Set<Point2D> {
        val stack = Stack<Current>()
        val energized = mutableSetOf<Point2D>()
        stack.push(start)
        val mem = mutableSetOf<Current>()
        while (!stack.isEmpty()) {
            val cur = stack.pop()
            if (cur in mem) {
                continue
            }
            mem.add(cur)
            val nextPos = cur.move()
            if (nextPos.x < 0 || nextPos.x > maxX || nextPos.y < 0 || nextPos.y > maxY) {
                continue
            }
            energized.add(nextPos)
            when (mirrors[nextPos]) {
                null -> stack.push(Current(cur.dir, nextPos))
                '-' -> {
                    if (cur.dir in setOf(Dir.N, Dir.S)) {
                        stack.push(Current(Dir.W, nextPos))
                        stack.push(Current(Dir.E, nextPos))
                    } else {
                        stack.push(Current(cur.dir, nextPos))
                    }
                }

                '|' -> {
                    if (cur.dir in setOf(Dir.E, Dir.W)) {
                        stack.push(Current(Dir.N, nextPos))
                        stack.push(Current(Dir.S, nextPos))
                    } else {
                        stack.push(Current(cur.dir, nextPos))
                    }
                }

                '/' -> {
                    val nextDir = when (cur.dir) {
                        Dir.N -> cur.dir.turnRight()
                        Dir.W -> cur.dir.turnLeft()
                        Dir.S -> cur.dir.turnRight()
                        Dir.E -> cur.dir.turnLeft()
                    }
                    stack.push(Current(nextDir, nextPos))
                }

                '\\' -> {
                    val nextDir = when (cur.dir) {
                        Dir.N -> cur.dir.turnLeft()
                        Dir.W -> cur.dir.turnRight()
                        Dir.S -> cur.dir.turnLeft()
                        Dir.E -> cur.dir.turnRight()
                    }
                    stack.push(Current(nextDir, nextPos))
                }

                else -> throw RuntimeException("Unknown mirror ${mirrors[nextPos]}")
            }
        }
        return energized
    }

    private fun part2(lines: List<String>): Any {
        val mirrors = readMirrors(lines)
        val maxX = lines[0].length - 1
        val maxY = lines.size - 1
        val starts = generatePossibleStarts(maxY, maxX)
        return starts.maxOf { getEnergized(it, maxX, maxY, mirrors).size }
    }

    private fun generatePossibleStarts(maxY: Int, maxX: Int): Set<Current> = ((0..maxY).flatMap { y ->
        setOf(
            Current(Dir.E, Point2D(-1, y)),
            Current(Dir.W, Point2D(maxX + 1, y)),
        )
    } + (0..maxX).flatMap { x ->
        setOf(
            Current(Dir.S, Point2D(x, -1)),
            Current(Dir.N, Point2D(x, maxY + 1)),
        )
    }).toSet()
}

