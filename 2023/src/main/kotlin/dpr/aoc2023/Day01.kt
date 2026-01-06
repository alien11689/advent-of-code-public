package dpr.aoc2023

import dpr.commons.Util

object Day01 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/01/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    @JvmStatic
    fun part1(lines: List<String>): Int {
        return lines.sumOf { line ->
            val digits = line.filter { it.isDigit() }
            Integer.parseInt("${digits[0]}${digits[digits.length - 1]}")
        }
    }

    @JvmStatic
    fun part2(lines: List<String>): Int {
        val mapping = mapOf(
            "one" to "1",
            "two" to "2",
            "three" to "3",
            "four" to "4",
            "five" to "5",
            "six" to "6",
            "seven" to "7",
            "eight" to "8",
            "nine" to "9",
            "zero" to "0",
        ) + (0..9).associate { "$it" to "$it" }
        return lines.sumOf { line ->
            val firstDigit =
                mapping.entries.filter { line.contains(it.key) }.minByOrNull { line.indexOf(it.key) }!!.value
            val lastDigit =
                mapping.entries.filter { line.contains(it.key) }.maxByOrNull { line.lastIndexOf(it.key) }!!.value
            Integer.parseInt("${firstDigit}${lastDigit}")
        }
    }
}
