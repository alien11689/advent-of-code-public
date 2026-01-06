package dpr.aoc2023

import dpr.commons.Dir
import dpr.commons.Point2D
import dpr.commons.Util
import kotlin.math.min

object Day18 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/18/input.txt")
        println(part1(lines))
        println(part2(lines))
        // it should be implementable with shoelace formula and pick's theorem but my solution just works
    }

    @JvmStatic
    fun part1(lines: List<String>): Long {
        val (border, borderSize) = readLines(lines) { line ->
            val (d, size) = line.split(Regex("[ )(#]+"))
            val dir = when (d) {
                "R" -> Dir.E
                "D" -> Dir.S
                "L" -> Dir.W
                "U" -> Dir.N
                else -> throw RuntimeException(d)
            }
            val length = size.toInt()
            dir to length
        }
        return calculateWholeSize(borderSize, border)
    }

    private fun readLines(lines: List<String>, parseLine: (String) -> Pair<Dir, Int>): Pair<MutableSet<Range>, Long> {
        val border = mutableSetOf<Range>()
        var cur = Point2D(0, 0)
        var borderSize = 0L
        lines.forEach { line ->
            val (dir, length) = parseLine(line)
            val next = cur.move(dir, length)
            if (cur < next) {
                border.add(Range(cur, next))
            } else {
                border.add(Range(next, cur))
            }
            cur = next
            borderSize += length
        }
        return Pair(border, borderSize)
    }

    data class Range(val start: Point2D, val end: Point2D) {
        fun upDown(): Boolean {
            return start.x == end.x
        }

        fun minY(): Int = min(start.y, end.y)

        fun containsY(y: Int): Boolean {
            return y in (start.y..end.y)
        }

        fun contains(p: Point2D): Boolean = p.x in start.x..end.x && p.y in start.y..end.y
    }

    @JvmStatic
    fun part2(lines: List<String>): Long {
        val (border, borderSize) = readLines(lines) { line ->
            val color = line.split(Regex("[ )(#]+"))[2]
            val length = color.take(5).toInt(16)
            val dir = when (color.last()) {
                '0' -> Dir.E
                '1' -> Dir.S
                '2' -> Dir.W
                '3' -> Dir.N
                else -> throw RuntimeException("Unknown")
            }
            dir to length
        }
        return calculateWholeSize(borderSize, border)
    }

    private fun calculateWholeSize(borderSize: Long, border: MutableSet<Range>): Long {
        var filledSize = borderSize
        val (upDown, leftRight) = border.partition { it.upDown() }
        val minY = border.minOf { it.minY() }
        val yWithLines = leftRight.map { it.start.y }.toSet().sorted()
        var lastY = minY
        for (y in yWithLines) {
            filledSize += calculateLineFill(y, upDown, leftRight)
            if (y - lastY > 1) {
                filledSize += (y - lastY - 1) * calculateLineFill(y - 1, upDown, leftRight).toLong()
            }
            lastY = y
        }
        return filledSize
    }

    private fun calculateLineFill(y: Int, upDown: List<Range>, leftRight: List<Range>): Int {
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
        return increment
    }
}
