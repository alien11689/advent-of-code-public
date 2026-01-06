package dpr.aoc2023

import dpr.commons.Util
import kotlin.math.sqrt

object Day06 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/06/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    @JvmStatic
    fun part1(lines: List<String>): Long {
        val times = lines[0].split(":")[1].trim().split(Regex("\\s+")).map { it.toInt() }
        val distances = lines[1].split(":")[1].trim().split(Regex("\\s+")).map { it.toInt() }
        return times.zip(distances).fold(1L) { acc, (t, d) -> acc * sail(t.toLong(), d.toLong()) }
    }

    private fun sail(time: Long, distance: Long): Int {
        return (1..time).count { speed ->
            speed * (time - speed) > distance
        }
    }

    @JvmStatic
    fun part2(lines: List<String>): Int {
        val time = lines[0].split(":")[1].trim().replace(Regex("\\s+"), "").toLong()
        val distance = lines[1].split(":")[1].trim().replace(Regex("\\s+"), "").toLong()
        return sail(time, distance)
    }
}

