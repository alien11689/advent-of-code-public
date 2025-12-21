package dpr.aoc2017

import dpr.commons.Util

object Day04 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/04/input.txt")
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: List<String>): Int {
        return input.map {
            it.split(" ")
        }.count {
            it.size == it.toSet().size
        }
    }

    @JvmStatic
    fun part2(input: List<String>): Int {
        return input.map { line ->
            line.split(" ").toList().map { it.toCharArray().sorted() }
        }.count {
            it.size == it.toSet().size
        }
    }
}
