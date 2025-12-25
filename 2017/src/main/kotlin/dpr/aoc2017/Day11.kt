package dpr.aoc2017

import dpr.commons.Point2D
import dpr.commons.Util
import kotlin.math.abs

object Day11 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/11/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: String): Int {
        val processSteps = input.split(",").toList()
        val dest = processSteps.fold(Point2D(0, 0)) { it, step ->
            nextStep(step, it)
        }

        return dist(dest)
    }

    @JvmStatic
    fun part2(input: String): Int {
        val processSteps = input.split(",").toList()
        var max = 0
        processSteps.fold(Point2D(0, 0)) { it, step ->
            val point = nextStep(step, it)
            val dist = dist(point)
            if (dist > max) {
                max = dist
            }
            point
        }
        return max
    }

    /**
     * Distances: https://www.redblobgames.com/grids/hexagons/#distances
     */
    private fun dist(a: Point2D, b: Point2D = Point2D(0, 0)): Int {
        val aq = a.x
        val ar = a.y - (a.x - (a.x and 1)) / 2

        val bq = b.x
        val br = b.y - (b.x - (b.x and 1)) / 2

        val aas = -aq - ar
        val bs = -bq - br

        return (abs(aq - bq) + abs(ar - br) + abs(aas - bs)) / 2
    }

    /**
     * Coordinates odd-q: https://www.redblobgames.com/grids/hexagons/#coordinates
     */
    private fun nextStep(step: String, it: Point2D) = when (step) {
        "n" -> Point2D(it.x, it.y - 1)
        "ne" -> Point2D(it.x + 1, if (it.x % 2 == 0) it.y - 1 else it.y)
        "nw" -> Point2D(it.x - 1, if (it.x % 2 == 0) it.y - 1 else it.y)
        "s" -> Point2D(it.x, it.y + 1)
        "se" -> Point2D(it.x + 1, if (it.x % 2 == 0) it.y else it.y + 1)
        "sw" -> Point2D(it.x - 1, if (it.x % 2 == 0) it.y else it.y + 1)
        else -> throw RuntimeException(step)
    }
}
