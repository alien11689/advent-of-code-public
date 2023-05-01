package dpr.aoc2019

import kotlin.math.absoluteValue

object Day10 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/10/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val points = mutableSetOf<Point>()

        for (i in input.indices) {
            for (j in input[i].indices) {
                if (input[i][j] == '#') {
                    points.add(Point(j, i))
                }
            }
        }

        val solution = points.maxOf { center ->
            (points.filter { it != center }.map { other ->
                listOf(calcA(center, other), signum(center.x, other.x), signum(center.y, other.y))
            }.toSet()).size
        }

        return solution
    }

    private fun part2(input: List<String>): Any {
        val points = mutableSetOf<Point>()

        for (i in input.indices) {
            for (j in input[i].indices) {
                if (input[i][j] == '#') {
                    points.add(Point(j, i))
                }
            }
        }

        val p2size = points.associateWith { center ->
            (points.filter { it != center }.map { other ->
                listOf<Any>(calcA(center, other), getOrd(center, other))
            }.toSet()).size
        }

        val center = p2size.maxByOrNull { it.value }!!.key

        val p2stats = points.filter { it != center }.associateWith { p ->
            Stats(getOrd(center, p), calcA(center, p))
        }

        val stats2p = mutableMapOf<Stats, MutableList<Point>>()
        p2stats.forEach {
            if (it.value in stats2p) {
                stats2p[it.value]!!.add(it.key)
            } else {
                stats2p[it.value] = mutableListOf(it.key)
            }
        }

        stats2p.values.forEach { l ->
            l.sortBy { manhattan(center, it) }
        }

        val stats = stats2p.keys.sorted()
        var i = 0

        while (i < 200) {
            val ps = stats2p[stats[i % stats.size]]!!
            val p = ps.removeAt(0)
            stats2p[stats[i % stats.size]] = ps
            if (++i == 200) {
                return p.x * 100 + p.y
            }
        }
        throw RuntimeException()
    }

    data class Point(val x: Int, val y: Int)

    enum class Ord {
        X_UP, RIGHT_UP, Y_RIGHT, RIGHT_DOWN, X_DOWN, LEFT_DOWN, Y_LEFT, LEFT_UP
    }

    private fun getOrd(p1: Point, p2: Point): Ord {
        if (p1.x == p2.x) {
            return if (p1.y < p2.y) Ord.X_DOWN else Ord.X_UP
        }
        if (p1.y == p2.y) {
            return if (p1.x < p2.x) Ord.Y_RIGHT else Ord.Y_LEFT
        }
        if (p1.x < p2.x) {
            return if (p1.y < p2.y) Ord.RIGHT_DOWN else Ord.RIGHT_UP
        }
        return if (p1.y < p2.y) Ord.LEFT_DOWN else Ord.LEFT_UP
    }

    private fun manhattan(p1: Point, p2: Point): Int {
        return (p1.x - p2.x).absoluteValue + (p1.y - p2.y).absoluteValue
    }

    private fun calcA(p1: Point, p2: Point): Double {
        return if (p1.x == p2.x) {
            1000000.0
        } else {
            (p2.y - p1.y).toDouble() / (p2.x - p1.x)
        }
    }

    private fun signum(a: Int, b: Int): Int {
        return if (a == b) {
            0
        } else if (a < b) {
            -1
        } else 1
    }

    data class Stats(val ord: Ord, val a: Double) : Comparable<Stats> {
        override fun compareTo(other: Stats): Int {
            if (ord == other.ord) {
                return when (ord) {
                    Ord.LEFT_UP -> a.compareTo(other.a)
                    Ord.LEFT_DOWN -> a.compareTo(other.a)
                    Ord.RIGHT_DOWN -> a.compareTo(other.a)
                    Ord.RIGHT_UP -> a.compareTo(other.a)
                    else -> throw RuntimeException()
                }
            }
            return ord.ordinal.compareTo(other.ord.ordinal)
        }
    }
}
