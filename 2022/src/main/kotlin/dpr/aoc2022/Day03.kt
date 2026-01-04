package dpr.aoc2022

import dpr.commons.Util

object Day03 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/03/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private val priorities = ('a'..'z') + ('A'..'Z')

    @JvmStatic
    fun part1(lines: List<String>): Long {
        return lines.sumOf { line ->
            val char = line.chunked(line.length / 2)
                .map { it.toSet() }
                .reduce { a, b -> a.intersect(b) }
                .single()
            priorities.indexOf(char) + 1L
        }
    }

    @JvmStatic
    fun part2(lines: List<String>): Long {
        return lines.chunked(3).sumOf { line ->
            val char = line.map { it.toSet() }
                .reduce { a, b -> a.intersect(b) }
                .single()
            priorities.indexOf(char) + 1L
        }
    }
}

