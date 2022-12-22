package pl.touk.dpr.aoc2021

import kotlin.math.absoluteValue

object Day07 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/07/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Long {
        val positions = lines[0].split(",").map { it.toInt() }
        var minFuel = positions.maxOrNull()!!.toLong() * positions.size
        for (i in positions.minOrNull()!!..positions.maxOrNull()!!) {
            val fuel = positions.sumOf { (it - i).absoluteValue.toLong() }
            if (fuel < minFuel) {
                minFuel = fuel
//                println("On $i there is $fuel")
            }
        }
        return minFuel
    }

    private fun part2(lines: List<String>): Long {
        val positions = lines[0].split(",").map { it.toInt() }
        var minFuel: Long? = null
        for (i in positions.minOrNull()!!..positions.maxOrNull()!!) {
            var fuel = 0L
            for (p in positions) {
                if (i == p) {
                    continue
                }
                var cur = 0
                for (x in 1..(p - i).absoluteValue) {
                    ++cur
                    fuel += cur
                }
            }
            if (minFuel == null || fuel < minFuel) {
                minFuel = fuel
//                println("On $i there is $fuel")
            }
        }
        return minFuel!!
    }

}

