package dpr.aoc2023

import dpr.commons.Dir
import dpr.commons.Point2D
import dpr.commons.Util
import java.util.Stack
import kotlin.math.max
import kotlin.math.min

object Day18 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/18/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/18/test1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val border = mutableMapOf<Point2D, String>()
        var cur = Point2D(0, 0)
        lines.forEach { line ->
            val (d, size, color) = line.split(Regex("[ )(#]+"))
            val dir = when (d) {
                "R" -> Dir.E
                "D" -> Dir.S
                "L" -> Dir.W
                "U" -> Dir.N
                else -> throw RuntimeException(d)
            }
            (1..size.toInt()).forEach {
                cur = cur.move(dir)
                border[cur] = color
            }
        }
//        (border.minOf { it.key.y }..border.maxOf { it.key.y }).forEach { y ->
//            (border.minOf { it.key.x }..border.maxOf { it.key.x }).forEach { x ->
//                val p = Point2D(x, y)
//                if (p in border) {
//                    print("#")
//                } else {
//                    print(" ")
//                }
//            }
//            println()
//        }
        var interior = findInterior(border)
        val s = Stack<Point2D>()
        s.add(interior)
        while (s.isNotEmpty()) {
            val cur = s.pop()
            if (cur in border) {
                continue
            }
            border[cur] = "0"
            listOf(cur.up(), cur.down(), cur.left(), cur.right()).forEach { s.push(it) }
        }
        return border.keys.size
    }

    private fun findInterior(border: MutableMap<Point2D, String>): Point2D {
        (border.minOf { it.key.y }..border.maxOf { it.key.y }).forEach { y ->
            (border.minOf { it.key.x }..border.maxOf { it.key.x }).forEach { x ->
                val p = Point2D(x, y)
                if (p.up() in border && p.left() in border) {
                    return p
                }
            }
        }
        throw RuntimeException()
    }

    data class Range(val start: Point2D, val end: Point2D) {
        fun upDown(): Boolean {
            return start.x == end.x
        }

        fun leftRight(): Boolean {
            return !upDown()
        }

        fun minY(): Int = min(start.y, end.y)
        fun maxY(): Int = max(start.y, end.y)
        fun minX(): Int = min(start.x, end.x)
        fun maxX(): Int = max(start.x, end.x)
        fun containsY(y: Int): Boolean {
            return y in (start.y..end.y)
        }

        fun contains(p: Point2D): Boolean = p.x in start.x..end.x && p.y in start.y..end.y
    }

    private fun part2(lines: List<String>): Any {
        val border = mutableSetOf<Range>()
        var filledSize = 0L
        var cur = Point2D(0, 0)
        lines.forEach { line ->
            val color = line.split(Regex("[ )(#]+"))[2]
            val d = color.take(5).toInt(16)
            val dir = when (color.last()) {
                '0' -> Dir.E
                '1' -> Dir.S
                '2' -> Dir.W
                '3' -> Dir.N
                else -> throw RuntimeException("Unknown")
            }
            val next = cur.move(dir, d)
            if (cur < next) {
                border.add(Range(cur, next))
            } else {
                border.add(Range(next, cur))
            }
            cur = next
            filledSize += d
        }
        val (upDown, leftRight) = border.partition { it.upDown() }
        val minY = border.minOf { it.minY() }
        val minX = border.minOf { it.minX() }
        val maxY = border.maxOf { it.maxY() }
        val maxX = border.maxOf { it.maxX() }
//        println("Area x ($minX, $maxX) y ($minY, $maxY)")
        for (y in (minY + 1)..<maxY) {
            val breaks = upDown.filter { it.containsY(y) }.map { it.start.x }.sorted()
            var outside = true
            var x = Int.MIN_VALUE
//            var prevOutside = true
            breaks.forEach { breakingX ->
                val range = Range(Point2D(x, y), Point2D(breakingX, y))
                val isLeftRight = range in leftRight
                if (isLeftRight) {
                    val startUpIsLine = upDown.any { it.contains(range.start.up()) }
                    val endUpIsLine = upDown.any { it.contains(range.end.up()) }
//                    prevOutside = outside
                    if (startUpIsLine == endUpIsLine) {
                        outside = !outside
                    } else {
//                        outside = !outside
                    }
                    x = breakingX
                } else if (outside) {
//                    prevOutside = outside
                    outside = false
                    x = breakingX
                } else {
//                    prevOutside = outside
                    filledSize += breakingX - 1 - x
                    outside = true
                    x = breakingX
                }
            }
            if (!outside) {
                throw RuntimeException("Error in line $y")
            }
//            if(containsLine) {
//                println("$y has breaks $breaks")
//            }
//            break
            println("Checking $y/$maxY")
        }
        return filledSize
    }
}

