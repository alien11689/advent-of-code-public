package dpr.aoc2021

import dpr.commons.Util
import dpr.commons.Point2D as Point

object Day17 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
//        val targetExample = Target(20..30, -10..-5)
        val target = Target(124..174, -123..-86)
        part1And2(target).forEach(::println)
    }

    @JvmStatic
    fun part1And2(target: Target): List<Int> {
        var maxHeight = 0
        var count = 0
        for (dx in 1..target.xRange.maxOrNull()!!) {
            for (dy in target.yRange.minOrNull()!!..1000) {
                val res = maxHeightOf(target, Velocity(dx, dy))
                if (res != null) {
                    ++count
                    if (res > maxHeight) {
                        maxHeight = res
                    }
                }
            }
        }
        return listOf(maxHeight, count)
    }

    private fun maxHeightOf(target: Target, velocity: Velocity): Int? {
        var velocity1 = velocity
        var maxHeight = 0
        var position = Point(0, 0)
        while (position.x !in target.xRange || position.y !in target.yRange) {
            position = Point(position.x + velocity1.dx, position.y + velocity1.dy)
            velocity1 = velocity1.next()
//            println("Position $position with velocity $velocity1")
            if (position.y > maxHeight) {
                maxHeight = position.y
            }
            if (position.y < target.yRange.minOrNull()!! || position.x > target.xRange.maxOrNull()!!) {
//                println("It missed target on $position not in $target")
                return null
            }
        }
        return maxHeight
    }

    data class Target(val xRange: IntRange, val yRange: IntRange)
    data class Velocity(val dx: Int, val dy: Int) {
        fun next(): Velocity {
            return Velocity(
                if (dx > 0) dx - 1 else if (dx < 0) dx + 1 else 0,
                dy - 1
            )
        }
    }
}


