package pl.touk.dpr.aoc2022

import pl.touk.dpr.aoc2022.Day22.Direction.D
import pl.touk.dpr.aoc2022.Day22.Direction.L
import pl.touk.dpr.aoc2022.Day22.Direction.R
import pl.touk.dpr.aoc2022.Day22.Direction.U
import java.util.StringTokenizer

object Day22 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getLinesFromFile("/22/input.txt")
        println("Part 1:")
//        println(part1(Util.getLinesFromFile("/22/test1.txt")))
        println(part1(lines))
        println("Part 2:")
        println(part2(Util.getLinesFromFile("/22/test1.txt")))
        println(part2(lines))
    }

    data class Point(val x: Int, val y: Int)

    enum class Elem {
        WALL, EMPTY
    }

    enum class Direction(val num: Int, val dx: Int, val dy: Int) {
        R(0, 1, 0), D(1, 0, 1), L(2, -1, 0), U(3, 0, -1);

        fun turn(side: String) = when (this) {
            R -> when (side) {
                "R" -> D
                "L" -> U
                else -> throw RuntimeException()
            }

            D -> when (side) {
                "R" -> L
                "L" -> R
                else -> throw RuntimeException()
            }

            L -> when (side) {
                "R" -> U
                "L" -> D
                else -> throw RuntimeException()
            }

            U -> when (side) {
                "R" -> R
                "L" -> L
                else -> throw RuntimeException()
            }
        }
    }

    data class Position(val point: Point, val facing: Direction) {
        fun turnLeft(): Position = copy(facing = facing.turn("L"))

        fun turnRight(): Position = copy(facing = facing.turn("R"))
        fun go1(steps: Int, map: MutableMap<Point, Elem>): Position {
            var cur = this
            repeat(steps) {
                var newPoint = cur.point.copy(x = cur.point.x + facing.dx, y = cur.point.y + facing.dy)
                if (newPoint !in map) {
                    newPoint = when (facing) {
                        R -> map.keys.filter { it.y == cur.point.y }.minBy { it.x }
                        D -> map.keys.filter { it.x == cur.point.x }.minBy { it.y }
                        L -> map.keys.filter { it.y == cur.point.y }.maxBy { it.x }
                        U -> map.keys.filter { it.x == cur.point.x }.maxBy { it.y }
                    }
                }
                cur = when (map[newPoint]) {
                    Elem.EMPTY -> cur.copy(point = newPoint)
                    Elem.WALL -> return cur
                    else -> throw RuntimeException()
                }
            }
            return cur
        }

        fun score() = 1000 * point.y + 4 * point.x + facing.num

        fun go2(steps: Int, map: MutableMap<Point, Elem>): Position {
            var cur = this
            var curFacing = cur.facing
            repeat(steps) {
                var newPoint = cur.point.copy(x = cur.point.x + curFacing.dx, y = cur.point.y + curFacing.dy)
                if (newPoint !in map) {
                    val curPoint = cur.point
                    if (map.keys.maxOf { it.y } == 12) {
                        // ..1.
                        // 234.
                        // ..56
                        when {
                            // sector 4 -> 6
                            curPoint.x == 12 && curPoint.y in 5..8 -> {
                                curFacing = D
                                // changing indices order
                                newPoint = Point(x = 12 + 9 - curPoint.y, y = 9)
                            }
                            // sector 5 down -> 2 up
                            curPoint.y == 12 && curPoint.x in 9..12 && curFacing == D -> {
                                curFacing = U
                                // changing indices
                                newPoint = Point(x = 0 + 13 - curPoint.x, y = 8)
                            }
                            // sector 3 U -> sector 1 R
                            curPoint.y == 5 && curPoint.x in 5..8 && curFacing == U -> {
                                curFacing = R
                                // changing indices
                                newPoint = Point(x = 9, y = curPoint.x - 4)
                            }

                            else -> throw RuntimeException("Model me $cur")
                        }
                    } else {
                        // .12
                        // .3.
                        // 45.
                        // 6..

                        when {
                            else -> throw RuntimeException("Model me $cur")
                        }
                    }
                    println("Wrapping from $curPoint to $newPoint facing $curFacing")
                }
                cur = when (map[newPoint]) {
                    Elem.EMPTY -> cur.copy(point = newPoint, facing = curFacing)
                    Elem.WALL -> return cur
                    else -> throw RuntimeException()
                }
            }
            return cur

        }
    }

    private fun part1(lines: List<String>): Any {
        val map = readMap(lines)
        var curPos = initPosition(map)
        val tokenizer = StringTokenizer(lines.last { it.isNotBlank() }, "LR", true)
        while (tokenizer.hasMoreTokens()) {
            curPos = when (val token = tokenizer.nextToken()) {
                "L" -> curPos.turnLeft()
                "R" -> curPos.turnRight()
                else -> curPos.go1(token.toInt(), map)
            }
//            println(curPos)
        }
        return curPos.score()
    }

    private fun readMap(lines: List<String>): MutableMap<Point, Elem> {
        val map = mutableMapOf<Point, Elem>()
        for (y in lines.indices) {
            if (lines[y].isBlank()) {
                break
            }
            for (x in lines[y].indices) {
                val cur = lines[y][x]
                val point = Point(x + 1, y + 1)
                if (cur == '#') {
                    map[point] = Elem.WALL
                } else if (cur == '.') {
                    map[point] = Elem.EMPTY
                }
            }
        }
        return map
    }

    private fun initPosition(map: MutableMap<Point, Elem>): Position {
        val minY = map.keys.minOf { it.y }
        val minX = map.keys.filter { it.y == minY }.minOf { it.x }
        return Position(Point(minX, minY), R)
    }

    private fun part2(lines: List<String>): Any {
        val map = readMap(lines)
        var curPos = initPosition(map)
        val tokenizer = StringTokenizer(lines.last { it.isNotBlank() }, "LR", true)
        while (tokenizer.hasMoreTokens()) {
            curPos = when (val token = tokenizer.nextToken()) {
                "L" -> curPos.turnLeft()
                "R" -> curPos.turnRight()
                else -> curPos.go2(token.toInt(), map)
            }
            println(curPos)
        }
        return curPos.score()
    }
}

