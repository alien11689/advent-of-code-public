package dpr.aoc2023

import kotlin.math.pow

object Day04 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/04/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        return lines.sumOf { line ->
            val (winning, my) = line.split(":")[1].split("|").map { it.trim().split(Regex("\\s+")) }
            val win = my.count { it in winning }
            if (win <= 0) 0 else 2.toDouble().pow(win - 1).toLong()
        }
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

