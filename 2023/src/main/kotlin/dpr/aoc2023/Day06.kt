package dpr.aoc2023

import dpr.commons.Util
import kotlin.math.sqrt

object Day06 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/06/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/06/test1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val times = lines[0].split(":")[1].trim().split(Regex("\\s+")).map { it.toInt() }
        val distances = lines[1].split(":")[1].trim().split(Regex("\\s+")).map { it.toInt() }
        return times.zip(distances).fold(1L) { acc, (t, d) -> acc * sail(t.toLong(), d.toLong()) }
    }

    private fun sail(time: Long, distance: Long): Int {
        return (1..time).count { speed ->
            speed * (time - speed) > distance
        }
    }

    @Suppress("unused")
    private fun sail2(time: Long, distance: Long): Long {
        // s * t - s*s - distance > 0
        // s*s - s*t + distance < 0
        // delta = t^2 - 4 * distance
        val deltaSqrt = sqrt(1.0 * time * time - 4 * distance)
        val speed1 = ((time - deltaSqrt) / 2).toLong()
        val speed2 = ((time + deltaSqrt) / 2).toLong()
//        return (speed1..speed2).count { speed ->
//            speed * (time - speed) > distance
//        }
        return speed2 - speed1
    }

    private fun part2(lines: List<String>): Any {
        val time = lines[0].split(":")[1].trim().replace(Regex("\\s+"), "").toLong()
        val distance = lines[1].split(":")[1].trim().replace(Regex("\\s+"), "").toLong()
        return sail(time, distance)
    }
}

