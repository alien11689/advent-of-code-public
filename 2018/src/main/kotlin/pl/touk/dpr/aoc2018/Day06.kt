package pl.touk.dpr.aoc2018

import kotlin.math.absoluteValue

object Day06 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/06/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        var counter = 'a'.code
        val points = input.map {
            val parts = it.split(", ")
            Point(parts[0].toInt(), parts[1].toInt(), ++counter)
        }

        val board = (0..(points.maxByOrNull { it.y }!!.y + 1)).map { y ->
            (0..(points.maxByOrNull { it.x }!!.x + 1)).map { x ->
                val distances = points.map { Pair(it.id, it.distance(x, y)) }.toMap()
                val min = distances.minByOrNull { it.value }!!.value
                val mins = distances.filter { it.value == min }
                val ids = mins.keys
                Cell1(ids)
            }
        }

        val mins1 = points.map { p ->
            val count = board.flatten().filter { c -> c.nearestPoints == setOf(p.id) }.size
            Pair(p.id, count)
        }.toMap()

        val board2 = (-10..(points.maxByOrNull { it.y }!!.y + 10)).map { y ->
            (-10..(points.maxByOrNull { it.x }!!.x + 10)).map { x ->
                val distances = points.map { Pair(it.id, it.distance(x, y)) }.toMap()
                val min = distances.minByOrNull { it.value }!!.value
                val mins = distances.filter { it.value == min }
                val ids = mins.keys
                Cell1(ids)
            }
        }

        val mins2 = points.map { p ->
            val count = board2.flatten().filter { c -> c.nearestPoints == setOf(p.id) }.size
            Pair(p.id, count)
        }.toMap()

        return mins1.filter { it.value == mins2[it.key] }.maxByOrNull { it.value }!!.value

    }

    private fun part2(input: List<String>): Any {
        var counter = 'a'.code
        val points = input.map {
            val parts = it.split(", ")
            Point(parts[0].toInt(), parts[1].toInt(), ++counter)
        }
        val board = (-5..(points.maxByOrNull { it.y }!!.y + 5)).map { y ->
            (-5..(points.maxByOrNull { it.x }!!.x + 5)).map { x ->
                val dist = points.sumOf { it.distance(x, y) }
                Cell2(x, y, dist)
            }
        }

        val inBound = board.flatten().filter { it.distance < 10000 }
        return inBound.size

    }

    data class Cell1(val nearestPoints: Set<Int>)

    data class Cell2(val x: Int, val y: Int, val distance: Int) {
        fun isNeighbour(o: Cell2): Boolean {
            return (o.x - this.x).absoluteValue + (o.y - this.y).absoluteValue <= 1
        }

        fun isPoint(x: Int, y: Int): Boolean {
            return this.x == x && this.y == y
        }
    }

    data class Point(val x: Int, val y: Int, val id: Int) {
        fun isPoint(x: Int, y: Int): Boolean {
            return this.x == x && this.y == y
        }

        fun distance(x: Int, y: Int): Int {
            return (this.x - x).absoluteValue + (this.y - y).absoluteValue
        }
    }
}
