package dpr.aoc2018

import dpr.commons.Util

object Day10 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/10/input.txt")
        val (part1, part2) = part1And2(input)
        println(part1)
        println(part2)
    }

    @JvmStatic
    fun part1And2(input: List<String>): Pair<String, Int> {
        val points = input.map {
            val line = it.split(Regex("[<>, ]+"))
            Point(
                x = line[1].toInt(),
                y = line[2].toInt(),
                vx = line[4].toInt(),
                vy = line[5].toInt(),
            )
        }
        var tick = 0
        var prevMax = Int.MAX_VALUE
        var bestTick = 0
        var bestPoints: List<Point> = listOf()
        while (true) {
            val minX = points.minByOrNull { it.x }!!.x
            val maxX = points.maxByOrNull { it.x }!!.x
            if (prevMax < maxX) {
                break
            }
            prevMax = maxX
            if (maxX - minX < 100) {
                bestTick = tick
                bestPoints = points.map { it.copy() }
            }
            ++tick
            points.forEach { it.move() }
        }
        val part1 = printPoints(bestPoints)
        return Pair(part1, bestTick)
    }

    data class Point(var x: Int, var y: Int, val vx: Int, val vy: Int) {
        fun move() {
            x += vx
            y += vy
        }
    }

    private fun printPoints(points: List<Point>): String {
        val minX = points.minByOrNull { it.x }!!.x
        val maxX = points.maxByOrNull { it.x }!!.x
        val minY = points.minByOrNull { it.y }!!.y
        val maxY = points.maxByOrNull { it.y }!!.y
        val ps = points.map { p -> listOf(p.x, p.y) }
        val sb = StringBuilder()
        for (y in (minY..maxY)) {
            for (x in (minX..maxX)) {
                if (ps.contains(listOf(x, y))) {
                    sb.append('#')
                } else {
                    sb.append('.')
                }
            }
            if (y < maxY) {
                sb.append("\n")
            }
        }
        return sb.toString()
    }
}
