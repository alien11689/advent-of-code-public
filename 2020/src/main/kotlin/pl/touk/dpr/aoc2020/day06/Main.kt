package pl.touk.dpr.aoc2020.day06

import pl.touk.dpr.aoc2020.Util

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getFileContent("/06/input.txt")
        part1(input)
        part2(input)
    }

    private fun part1(input: String) {
        val lines = input.lines()
        var sum = 0
        var current = mutableSetOf<Char>()
        lines.forEach { line ->
            if (line.isEmpty()) {
                sum += current.size
                current = mutableSetOf()
            } else {
                current.addAll(line.toCharArray().toList())
            }
        }
        println(sum)
    }

    private fun part2(input: String) {
        val lines = input.lines()
        var sum = 0
        var current = ('a'..'z').toSet()
        lines.forEach { line ->
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

