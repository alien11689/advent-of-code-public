package dpr.aoc2023

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
        val mirrors = mutableMapOf<Point2D, Char>()
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                if (c != '.') {
                    mirrors[Point2D(x, y)] = c
                }
            }
        }
        val maxX = lines[0].length - 1
        val maxY = lines.size - 1
        val stack = Stack<Current>()
        val energized = mutableSetOf<Point2D>()
        stack.push(Current(Dir.E, Point2D(-1, 0)))
        val mem = mutableSetOf<Current>()
        while (!stack.isEmpty()) {
            val cur = stack.pop()
//            println("Checking $cur")
            if (cur in mem) {
                continue
            }
            mem.add(cur)
            val nextPos = cur.move()
            if (nextPos.x < 0 || nextPos.x > maxX || nextPos.y < 0 || nextPos.y > maxY) {
                continue
            }
            energized.add(nextPos)
            val mirror = mirrors[nextPos]
            when (mirror) {
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

                else -> throw RuntimeException("Unknown mirror $mirror")
            }
        }
        return energized.size
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

