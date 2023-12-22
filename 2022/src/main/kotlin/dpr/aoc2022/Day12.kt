package dpr.aoc2022

import dpr.commons.Util
import java.util.PriorityQueue
import dpr.commons.Point2D as Point

object Day12 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/12/input.txt")
//        println("Part 1:")
//        println(part1(Util.getNotEmptyLinesFromFile("/12/test1.txt")))
        println(part1(lines))
//        println("Part 2:")
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val points2 = readPoints(lines)
        val start = points2.filter { it.value == 'S' }.keys.first()
        val target = points2.filter { it.value == 'E' }.keys.first()
        val points = normalizePoints(points2)
        return shortestPath(start, points, target)
    }

    private fun shortestPath(start: Point, points: Map<Point, Char>, target: Point): Int {
        val pq = PriorityQueue<State>()
        pq.offer(State(start, 'a', 0))
        val mem = mutableSetOf<Point>()
        while (pq.isNotEmpty()) {
            val cur = pq.poll()
            cur.neighbours(points)
                .forEach {
                    if (it.p == target) {
                        return it.steps
                    }
                    if (it.p !in mem) {
                        mem.add(it.p)
                        pq.offer(it)
                    }
                }
        }
        return Int.MAX_VALUE
    }

    data class State(val p: Point, val height: Char, val steps: Int) : Comparable<State> {
        override fun compareTo(other: State): Int = steps - other.steps
        fun neighbours(points: Map<Point, Char>): Set<State> {
            return p.neighboursCross()
                .filter { it in points }
                .filter {
                    val nextHeight = points[it]!!
                    when (height) {
                        else -> nextHeight == height + 1 || nextHeight <= height
                    }
                }
                .map { State(it, points[it]!!, steps + 1) }
                .toSet()
        }
    }

    private fun part2(lines: List<String>): Any {
        val points2 = readPoints(lines)
        val target = points2.filter { it.value == 'E' }.keys.first()
        val points = normalizePoints(points2)
        return points.filter { it.value == 'a' }
            .map { shortestPath(it.key, points, target) }
            .min()
    }

    private fun readPoints(lines: List<String>): Map<Point, Char> =
        lines.flatMapIndexed { y, line ->
            line.mapIndexed { x, c ->
                Point(x, y) to c
            }
        }.toMap()

    private fun normalizePoints(points2: Map<Point, Char>): Map<Point, Char> {
        return points2.mapValues {
            when (it.value) {
                'S' -> 'a'
                'E' -> 'z'
                else -> it.value
            }
        }
    }
}

