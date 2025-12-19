package dpr.aoc2016

import dpr.commons.Util

object Day12 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/12/input.txt")
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: List<String>): Int {
        return Assembunny(input).run().first["a"]!!
    }

    private fun part2(input: List<String>): Int {
        return Assembunny(input).run(mapOf(Pair("c", 1))).first["a"]!!
    }
}
