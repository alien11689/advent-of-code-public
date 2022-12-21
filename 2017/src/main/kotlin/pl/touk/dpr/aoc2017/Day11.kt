package pl.touk.dpr.aoc2017

import java.math.BigDecimal

object Day11 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/11/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val processSteps = input.split(",").toList()
        val bd05 = BigDecimal("0.5")
        val dest = processSteps.fold(Point(BigDecimal.ZERO, BigDecimal.ZERO)) { it, step ->
            when (step) {
                "n" -> Point(it.x, it.y - BigDecimal.ONE)
                "ne" -> Point(it.x + bd05, it.y - bd05)
                "nw" -> Point(it.x - bd05, it.y - bd05)
                "s" -> Point(it.x, it.y + BigDecimal.ONE)
                "se" -> Point(it.x + bd05, it.y + bd05)
                "sw" -> Point(it.x - bd05, it.y + bd05)
                else -> throw RuntimeException(step)
            }
        }
        return (dest.x / bd05 + dest.y - dest.x).toInt()
    }

    private fun part2(input: String): Any {
        val processSteps = input.split(",").toList()
        val bd05 = BigDecimal("0.5")
        var max = BigDecimal.ZERO
        val dest = processSteps.fold(Point(BigDecimal.ZERO, BigDecimal.ZERO)) { it, step ->
            val point = when (step) {
                "n" -> Point(it.x, it.y - BigDecimal.ONE)
                "ne" -> Point(it.x + bd05, it.y - bd05)
                "nw" -> Point(it.x - bd05, it.y - bd05)
                "s" -> Point(it.x, it.y + BigDecimal.ONE)
                "se" -> Point(it.x + bd05, it.y + bd05)
                "sw" -> Point(it.x - bd05, it.y + bd05)
                else -> throw RuntimeException(step)
            }
            val dist = (point.x / bd05 + point.y - point.x)
            if (dist > max) {
                max = dist
            }
            point
        }
        return max.toInt()
    }

    data class Point(val x: BigDecimal, val y: BigDecimal)
}
