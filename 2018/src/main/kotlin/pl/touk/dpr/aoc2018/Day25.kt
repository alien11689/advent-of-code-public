package pl.touk.dpr.aoc2018

import java.util.Stack
import kotlin.math.abs

object Day25 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/25/input.txt")
        println(part1(input))
    }

    private fun part1(input: List<String>): Any {
        return findConstellation(readPoints(input)).size
    }

    data class Point(var w: Int, var x: Int, var y: Int, var z: Int) {
        fun manhattan(o: Point): Int {
            return abs(w - o.w) + abs(x - o.x) + abs(y - o.y) + abs(z - o.z)
        }
    }

    private fun readPoints(lines: List<String>): List<Point> {
        return lines.map { line ->
            val parts = line.split(Regex("[ ,]+")).map { it.toInt() }
            Point(parts[0], parts[1], parts[2], parts[3])
        }
    }

    private fun findConstellation(initPoints: List<Point>): Set<Set<Point>> {
        val points = initPoints.toMutableList()
        val constellations = mutableSetOf<Set<Point>>()
        while (points.isNotEmpty()) {
            val constel = mutableSetOf<Point>()
            val neighbours = Stack<Point>()
            val root = points.first()
            points.remove(root)
            neighbours.push(root)
            constel.add(root)
            while (!neighbours.isEmpty()) {
                val next = neighbours.pop()
                val nextNeighbours = points.filter { it.manhattan(next) <= 3 }
                neighbours.addAll(nextNeighbours)
                constel.addAll(nextNeighbours)
                points.removeAll(nextNeighbours)
            }
            constellations.add(constel)
        }
        return constellations
    }
}