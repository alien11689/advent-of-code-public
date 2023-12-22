package dpr.aoc2022

import dpr.commons.Dir
import dpr.commons.Dir.E
import dpr.commons.Dir.N
import dpr.commons.Dir.S
import dpr.commons.Dir.W
import dpr.commons.Util
import java.util.StringTokenizer
import dpr.commons.Point2D as Point

object Day22 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getLinesFromFile("/22/input.txt")
//        println("Part 1:")
//        println(part1(Util.getLinesFromFile("/22/test1.txt")))
        println(part1(lines))
//        println("Part 2:")
//        println(part2(Util.getLinesFromFile("/22/test1.txt")))
        println(part2(lines))
        // 132088 is too low
    }

    enum class Elem {
        WALL, EMPTY
    }

    private fun Dir.num() = when (this) {
        E -> 0
        S -> 1
        W -> 2
        N -> 3
    }

    data class Position(val point: Point, val facing: Dir) {
        fun turnLeft(): Position = copy(facing = facing.turnLeft())

        fun turnRight(): Position = copy(facing = facing.turnRight())

        fun go(steps: Int, map: MutableMap<Point, Elem>, handleWrap: (Position) -> Position): Position {
            var cur = this
            var curFacing = this.facing
            repeat(steps) {
                var newPoint = cur.point.copy(x = cur.point.x + curFacing.dx, y = cur.point.y + curFacing.dy)
                if (newPoint !in map) {
                    val wrappedPosition = handleWrap(cur)
                    newPoint = wrappedPosition.point
                    curFacing = wrappedPosition.facing
                }
                cur = when (map[newPoint]) {
                    Elem.EMPTY -> cur.copy(point = newPoint, facing = curFacing)
                    Elem.WALL -> return cur
                    else -> throw RuntimeException()
                }
            }
            return cur
        }

        fun score() = 1000 * point.y + 4 * point.x + facing.num()

    }

    private fun part1(lines: List<String>): Any {
        val map = readMap(lines)
        val handleWrap: (Position) -> Position = { cur ->
            Position(facing = cur.facing, point = when (cur.facing) {
                E -> map.keys.filter { it.y == cur.point.y }.minBy { it.x }
                S -> map.keys.filter { it.x == cur.point.x }.minBy { it.y }
                W -> map.keys.filter { it.y == cur.point.y }.maxBy { it.x }
                N -> map.keys.filter { it.x == cur.point.x }.maxBy { it.y }
            })
        }
        return traverseMap(map, lines, handleWrap).score()
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
        return Position(Point(minX, minY), E)
    }

    private fun part2(lines: List<String>): Any {
        val map = readMap(lines)
        val handleWrap = getHandleWrapPart2(map)
        return traverseMap(map, lines, handleWrap).score()
    }

    private fun traverseMap(map: MutableMap<Point, Elem>, lines: List<String>, handleWrap: (Position) -> Position): Position {
        var curPos = initPosition(map)
        val tokenizer = StringTokenizer(lines.last { it.isNotBlank() }, "LR", true)
        while (tokenizer.hasMoreTokens()) {
            curPos = when (val token = tokenizer.nextToken()) {
                "L" -> curPos.turnLeft()
                "R" -> curPos.turnRight()
                else -> curPos.go(token.toInt(), map, handleWrap)
            }
//            println(curPos)
        }
        return curPos
    }

    private fun getHandleWrapPart2(map: MutableMap<Point, Elem>): (Position) -> Position = { cur ->
        val curPoint = cur.point
        if (map.keys.maxOf { it.y } == 12) {
            // ..1.
            // 234.
            // ..56
            when {
                // sector 4 R -> 6 D
                curPoint.x == 12 && curPoint.y in 5..8 && cur.facing == E -> Position(Point(x = 12 + 9 - curPoint.y, y = 9), S)
                // sector 5 D -> 2 U
                curPoint.y == 12 && curPoint.x in 9..12 && cur.facing == S -> Position(Point(x = 0 + 13 - curPoint.x, y = 8), N)
                // sector 3 U -> 1 R
                curPoint.y == 5 && curPoint.x in 5..8 && cur.facing == N -> Position(Point(x = 9, y = curPoint.x - 4), E)
                else -> throw RuntimeException("Model me $cur")
            }
        } else {
            // .12
            // .3.
            // 45.
            // 6..
//            println("Modeling $cur -> sector xx=${(curPoint.x - 1) / 50} yy=${(curPoint.y - 1) / 50}")
            when {
                // sector 1 L -> 4 R with change indices
                curPoint.x == 51 && curPoint.y in 1..50 && cur.facing == W -> Position(Point(x = 1, y = 151 - curPoint.y), E)
                // sector 6 R -> 5 U without changing indices
                curPoint.x == 50 && curPoint.y in 151..200 && cur.facing == E -> Position(Point(x = curPoint.y - 100, y = 150), N)
                // sector 3 L -> 4 D without changing indices
                curPoint.x == 51 && curPoint.y in 51..100 && cur.facing == W -> Position(Point(x = curPoint.y - 50, y = 101), S)
                // sector 5 R -> 2 L with changing indices
                curPoint.x == 100 && curPoint.y in 101..150 && cur.facing == E -> Position(Point(x = 150, y = 151 - curPoint.y), W)
                // sector 2 D -> 3 L without
                curPoint.y == 50 && curPoint.x in 101..150 && cur.facing == S -> Position(Point(x = 100, y = curPoint.x - 50), W)
                // sector 2 R -> 5 L with
                curPoint.x == 150 && curPoint.y in 1..50 && cur.facing == E -> Position(Point(x = 100, y = 151 - curPoint.y), W)
                // sector 3 R -> 2 U without
                curPoint.x == 100 && curPoint.y in 51..100 && cur.facing == E -> Position(Point(x = 50 + curPoint.y, y = 50), N)
                // sector 1 U -> 6 R without
                curPoint.y == 1 && curPoint.x in 51..100 && cur.facing == N -> Position(Point(x = 1, y = 100 + curPoint.x), E)
                // sector 6 D -> 2 D without
                curPoint.y == 200 && curPoint.x in 1..50 && cur.facing == S -> Position(Point(x = 100 + curPoint.x, y = 1), S)
                // sector 4 L -> 1 R with
                curPoint.x == 1 && curPoint.y in 101..150 && cur.facing == W -> Position(Point(x = 51, y = 151 - curPoint.y), E)
                // sector 6 L -> 1 D without
                curPoint.x == 1 && curPoint.y in 151..200 && cur.facing == W -> Position(Point(x = curPoint.y - 100, y = 1), S)
                // sector 4 U -> 3 R without
                curPoint.y == 101 && curPoint.x in 1..50 && cur.facing == N -> Position(Point(x = 51, y = curPoint.x + 50), E)
                // sector 5 D -> 6 L without
                curPoint.y == 150 && curPoint.x in 51..100 && cur.facing == S -> Position(Point(x = 50, y = curPoint.x + 100), W)
                // sector 2 U -> 6 U without
                curPoint.y == 1 && curPoint.x in 101..150 && cur.facing == N -> Position(Point(x = curPoint.x - 100, y = 200), N)
                else -> throw RuntimeException("Model me $cur -> sector xx=${(curPoint.x - 1) / 50} yy=${(curPoint.y - 1) / 50}")
            }
        }
//        println("Wrapping from $curPoint to $newPoint facing $curFacing (xx=${(newPoint.x - 1) / 50} yy=${(newPoint.y - 1) / 50})")
    }
}

