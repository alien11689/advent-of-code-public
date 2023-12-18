package dpr.aoc2023

import dpr.commons.Dir
import dpr.commons.Point2D
import dpr.commons.Util
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
        val (border, borderSize) = readLines(lines)
        return calculateWholeSize(borderSize, border)
    }

    private fun readLines(lines: List<String>): Pair<MutableSet<Range>, Long> {
        val border = mutableSetOf<Range>()
        var cur = Point2D(0, 0)
        var borderSize = 0L
        lines.forEach { line ->
            val (d, size) = line.split(Regex("[ )(#]+"))
            val dir = when (d) {
                "R" -> Dir.E
                "D" -> Dir.S
                "L" -> Dir.W
                "U" -> Dir.N
                else -> throw RuntimeException(d)
            }
            val next = cur.move(dir, size.toInt())
            if (cur < next) {
                border.add(Range(cur, next))
            } else {
                border.add(Range(next, cur))
            }
            cur = next
            borderSize += size.toInt()
        }
        return Pair(border, borderSize)
    }


    private fun findInterior(border: Set<Point2D>): Point2D {
        (border.minOf { it.y }..border.maxOf { it.y }).forEach { y ->
            (border.minOf { it.x }..border.maxOf { it.x }).forEach { x ->
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

        fun minY(): Int = min(start.y, end.y)
        fun maxY(): Int = max(start.y, end.y)
        fun containsY(y: Int): Boolean {
            return y in (start.y..end.y)
        }

        fun contains(p: Point2D): Boolean = p.x in start.x..end.x && p.y in start.y..end.y
    }

    private fun part2(lines: List<String>): Any {
        val (border, borderSize) = readLines2(lines)
        return calculateWholeSize(borderSize, border)
    }

    private fun readLines2(lines: List<String>): Pair<MutableSet<Range>, Long> {
        val border = mutableSetOf<Range>()
        var cur = Point2D(0, 0)
        var borderSize = 0L
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
            borderSize += d
        }
        return Pair(border, borderSize)
    }

    private fun calculateWholeSize(borderSize: Long, border: MutableSet<Range>): Long {
        var filledSize = borderSize
        val (upDown, leftRight) = border.partition { it.upDown() }
        val minY = border.minOf { it.minY() }
        val maxY = border.maxOf { it.maxY() }
        var prevLineFill = -1
        val yWithLines = leftRight.map { it.start.y }.toSet()
        for (y in (minY + 1)..<maxY) {
            if (y !in yWithLines && prevLineFill >= 0) {
                filledSize += prevLineFill
                continue
            }
            var increment = 0
            val breaks = upDown.filter { it.containsY(y) }.map { it.start.x }.sorted()
            var outside = true
            var x = Int.MIN_VALUE
            breaks.forEach { breakingX ->
                val range = Range(Point2D(x, y), Point2D(breakingX, y))
                val isLeftRight = range in leftRight
                if (isLeftRight) {
                    val startUpIsLine = upDown.any { it.contains(range.start.up()) }
                    val endUpIsLine = upDown.any { it.contains(range.end.up()) }
                    if (startUpIsLine == endUpIsLine) {
                        // when start up and end up is the same it means that we need to continue prev status of line,
                        // but we already switched it on breaking point so let's recover
                        outside = !outside
                    }
                    x = breakingX
                } else if (outside) {
                    outside = false
                    x = breakingX
                } else {
                    increment += breakingX - 1 - x
                    outside = true
                    x = breakingX
                }
            }
            if (!outside) { // it
                throw RuntimeException("Error in line $y")
            }
            prevLineFill = if (y in yWithLines) -1 else increment
            filledSize += increment
            //            println("Checking $y/$maxY")
        }
        return filledSize
    }
}

