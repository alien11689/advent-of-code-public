package pl.touk.dpr.aoc2020.day06

import pl.touk.dpr.aoc2020.Util

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getLinesFromFile("/06/input.txt")
        part1(input)
        part2(input)
    }

    private fun part1(input: List<String>) {
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
        println(sum)
    }

    private fun part2(input: List<String>) {
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
        println(sum)
    }
}