package dpr.aoc2020

import dpr.commons.Util

object Day06 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getLinesFromFile("/06/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        var sum = 0
        var current = setOf<Char>()
        input.forEach { line ->
            if (line.isEmpty()) {
                sum += current.size
                current = setOf()
            } else {
                current = current + line.toCharArray().toSet()
            }
        }
        return sum
    }

    private fun part2(input: List<String>): Any {
        var sum = 0
        var current = ('a'..'z').toSet()
        input.forEach { line ->
            if (line.isEmpty()) {
                sum += current.size
                current = ('a'..'z').toMutableSet()
            } else {
                val inLine = line.toCharArray().toSet()
                current = current.intersect(inLine)
            }
        }
        return sum
    }
}
