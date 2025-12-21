package dpr.aoc2022

import dpr.commons.Util

object Day01 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getLinesFromFile("/01/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    @JvmStatic
    fun part1(lines: List<String>): Any {
        return elvesCaloriesDescending(lines).first()
    }

    @JvmStatic
    fun part2(lines: List<String>): Any {
        return elvesCaloriesDescending(lines).take(3).sum()
    }

    private fun elvesCaloriesDescending(lines: List<String>): List<Long> {
        val elves = mutableListOf<Long>()
        var current = 0L
        lines.forEach {
            if (it.isBlank()) {
                elves.add(current)
                current = 0L
            } else {
                current += it.toInt()
            }
        }
        elves.add(current)
        return elves.sortedBy { -it }
    }
}

