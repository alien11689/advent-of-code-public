package dpr.aoc2023

import dpr.commons.Dir
import dpr.commons.Point2D
import dpr.commons.Util
import java.util.Stack

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

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

