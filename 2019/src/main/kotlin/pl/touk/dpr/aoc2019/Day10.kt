package pl.touk.dpr.aoc2019

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

        val solution = points.map { center ->
            (points.filter { it != center }.map { other ->
                listOf(calcA(center, other), signum(center.x, other.x), signum(center.y, other.y))
            }.toSet()).size
        }.maxOrNull()!!

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

        val p2size = points.map { center ->
            val size = (points.filter { it != center }.map { other ->
                listOf(calcA(center, other), getOrd(center, other))
            }.toSet()).size
            center to size
        }.toMap()

        val center = p2size.maxByOrNull { it.value }!!.key

        val p2stats = points.filter { it != center }.map { p ->
            p to Stats(getOrd(center, p), calcA(center, p))
        }.toMap()

        val stats2p = mutableMapOf<Stats, MutableList<Point>>()
        p2stats.forEach {
            if (it.value in stats2p) {
                stats2p[it.value]!!.add(it.key)
            } else {
                stats2p[it.value] = mutableListOf(it.key)
            }
        }

        stats2p.values.forEach() { l ->
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

    fun getOrd(p1: Point, p2: Point): Ord {
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

    fun manhattan(p1: Point, p2: Point): Int {
        return (p1.x - p2.x).absoluteValue + (p1.y - p2.y).absoluteValue
    }

    fun calcA(p1: Point, p2: Point): Double {
        if (p1.x == p2.x) {
            return 1000000.0
        } else {
            return (p2.y - p1.y).toDouble() / (p2.x - p1.x)
        }
    }

    fun signum(a: Int, b: Int): Int {
        if (a == b) {
            return 0
        }
        if (a < b) {
            return -1
        }
        return 1
    }

    data class Stats(val ord: Ord, val a: Double) : Comparable<Stats> {
        override fun compareTo(o: Stats): Int {
            if (ord == o.ord) {
                return when (ord) {
                    Ord.LEFT_UP -> a.compareTo(o.a)
                    Ord.LEFT_DOWN -> a.compareTo(o.a)
                    Ord.RIGHT_DOWN -> a.compareTo(o.a)
                    Ord.RIGHT_UP -> a.compareTo(o.a)
                    else -> throw RuntimeException()
                }
            }
            return ord.ordinal.compareTo(o.ord.ordinal)
        }
    }
}
