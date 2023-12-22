package dpr.aoc2022

import dpr.commons.Util
import java.util.Stack
import dpr.commons.Point3D as Point

object Day18 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/18/input.txt")
//        println("Part 1:")
//        println(part1(Util.getNotEmptyLinesFromFile("/18/test0.txt")))
//        println(part1(Util.getNotEmptyLinesFromFile("/18/test1.txt")))
        println(part1(lines))
//        println("Part 2:")
//        println(part2(Util.getNotEmptyLinesFromFile("/18/test1.txt")))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val points = readPoints(lines)
        val surface = points.flatMap { it.neighboursCross() - points }
        return surface.size
    }

    private fun readPoints(lines: List<String>) = lines.map { line ->
        val (x, y, z) = line.split(",").map { it.toInt() }
        Point(x, y, z)
    }.toSet()

    private fun part2(lines: List<String>): Any {
        val points = readPoints(lines)
        val surface = points.flatMap { it.neighboursCross() - points }
        val surfacePoints = surface.toSet()
        val origin = surface.minBy { it.x }
        val externalSurface = mutableSetOf<Point>()
        val checkedPoints = mutableSetOf<Point>()
        val stack = Stack<Point>()
        stack.push(origin)
        externalSurface.add(origin)
        while (stack.isNotEmpty()) {
            val cur = stack.pop()
            if (cur in checkedPoints) {
                continue
            }
            checkedPoints.add(cur)
            cur.neighboursCross().forEach { neighbour ->
                if (neighbour in surfacePoints) {
                    externalSurface.add(neighbour)
                    if (neighbour !in checkedPoints) {
                        stack.push(neighbour)
                    }
                } else if (cur in surfacePoints && neighbour !in points) {
                    if (neighbour !in checkedPoints) {
                        stack.push(neighbour)
                    }
                }
            }
        }

        return surface.filter { it in externalSurface }.size
    }
}

