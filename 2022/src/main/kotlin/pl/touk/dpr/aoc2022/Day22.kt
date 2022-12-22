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
//        println(part2(Util.getLinesFromFile("/22/test1.txt")))
        println(part2(lines))
        // 132088 is too low
    }

    data class Point(val x: Int, val y: Int)

    enum class Elem {
        WALL, EMPTY
    }

    enum class Direction(val num: Int, val dx: Int, val dy: Int) {
        R(0, 1, 0), D(1, 0, 1), L(2, -1, 0), U(3, 0, -1);

        fun turnLeft() = when (this) {
            R -> U
            D -> R
            L -> D
            U -> L
        }

        fun turnRight() = when (this) {
            R -> D
            D -> L
            L -> U
            U -> R
        }
    }

    data class Position(val point: Point, val facing: Direction) {
        fun turnLeft(): Position = copy(facing = facing.turnLeft())

        fun turnRight(): Position = copy(facing = facing.turnRight())
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

//                        println("Modeling $cur -> sector xx=${(curPoint.x - 1) / 50} yy=${(curPoint.y - 1) / 50}")
                        when {
                            // sector 1 L -> 4 R with change indices
                            curPoint.x == 51 && curPoint.y in 1..50 && curFacing == L -> {
                                curFacing = R
                                newPoint = Point(x = 1, y = 151 - curPoint.y)
                            }
//                            // sector 6 R -> 5 U without changeing indices
                            curPoint.x == 50 && curPoint.y in 151..200 && curFacing == R -> {
                                curFacing = U
                                newPoint = Point(x = curPoint.y - 100, y = 150)
                            }
//                            // sector 3 L -> 4 D without changing indices
                            curPoint.x == 51 && curPoint.y in 51..100 && curFacing == L -> {
                                curFacing = D
                                newPoint = Point(x = curPoint.y - 50, y = 101)
                            }
//                            // sector 5 R -> 2 L with changing indices
                            curPoint.x == 100 && curPoint.y in 101..150 && curFacing == R -> {
                                curFacing = L
                                newPoint = Point(x = 150, y = 151 - curPoint.y)
                            }
//                            // sector 2 D -> 3 L without
                            curPoint.y == 50 && curPoint.x in 101..150 && curFacing == D -> {
                                curFacing = L
                                newPoint = Point(x = 100, y = curPoint.x - 50)
                            }
//                            // sector 2 R -> 5 L with
                            curPoint.x == 150 && curPoint.y in 1..50 && curFacing == R -> {
                                curFacing = L
                                // y 1..50 to y 150..101
                                newPoint = Point(x = 100, y = 151 - curPoint.y)
                            }
//                            // sector 3 R -> 2 U without
                            curPoint.x == 100 && curPoint.y in 51..100 && curFacing == R -> {
                                curFacing = U
                                newPoint = Point(x = 50 + curPoint.y, y = 50)
                            }
//                            // sector 1 U -> 6 R without
                            curPoint.y == 1 && curPoint.x in 51..100 && curFacing == U -> {
                                curFacing = R
                                newPoint = Point(x = 1, y = 100 + curPoint.x)
                            }
//                            // sector 6 D -> 2 D without
                            curPoint.y == 200 && curPoint.x in 1..50 && curFacing == D -> {
                                curFacing = D
                                newPoint = Point(x = 100 + curPoint.x, y = 1)
                            }
                            // fixed!!!
//                            // sector 4 L -> 1 R with
                            curPoint.x == 1 && curPoint.y in 101..150 && curFacing == L -> {
                                curFacing = R
                                newPoint = Point(x = 51, y = 151 - curPoint.y)
                            }
//                            // sector 6 L -> 1 D without
                            curPoint.x == 1 && curPoint.y in 151..200 && curFacing == L -> {
                                curFacing = D
                                newPoint = Point(x = curPoint.y - 100, y = 1)
                            }
//                            // sector 4 U -> 3 R without
                            curPoint.y == 101 && curPoint.x in 1..50 && curFacing == U -> {
                                curFacing = R
                                newPoint = Point(x = 51, y = curPoint.x + 50)
                            }
//                            // sector 5 D -> 6 L without
                            curPoint.y == 150 && curPoint.x in 51..100 && curFacing == D -> {
                                curFacing = L
                                newPoint = Point(x = 50, y = curPoint.x + 100)
                            }
//                            // sector 2 U -> 6 U without
                            curPoint.y == 1 && curPoint.x in 101..150 && curFacing == U -> {
                                curFacing = U
                                newPoint = Point(x = curPoint.x - 100, y = 200)
                            }

                            else -> throw RuntimeException("Model me $cur -> sector xx=${(curPoint.x - 1) / 50} yy=${(curPoint.y - 1) / 50}")
                        }
                    }
//                    println("Wrapping from $curPoint to $newPoint facing $curFacing (xx=${(newPoint.x - 1) / 50} yy=${(newPoint.y - 1) / 50})")
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
//            println(curPos)
        }
        return curPos.score()
    }
}

