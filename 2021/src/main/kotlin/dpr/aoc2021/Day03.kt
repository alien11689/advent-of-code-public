package dpr.aoc2021

import dpr.commons.Util

object Day03 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/03/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    @JvmStatic
    fun part1(lines: List<String>): Int {
        var gamma = ""
        var epsilon = ""
        for (i in lines[0].indices) {
            val ones = lines.map { it[i] }.count { it == '1' }
            val zeros = lines.map { it[i] }.count { it == '0' }
            if (ones > zeros) {
                gamma += "1"
                epsilon += "0"
            } else {
                gamma += "0"
                epsilon += "1"
            }
        }
        return Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)
    }

    @JvmStatic
    fun part2(lines: List<String>): Int {
        val oxygen = procesInput2(lines) { ones, zeros -> ones >= zeros }
        val co2 = procesInput2(lines) { ones, zeros -> ones < zeros }
        return Integer.parseInt(oxygen, 2) * Integer.parseInt(co2, 2)
    }

    private fun procesInput2(lines: List<String>, predicate: (Int, Int) -> Boolean): String {
        var result = ""
        var numbers = lines
        for (i in lines[0].indices) {
            if (numbers.size == 1) {
                result = numbers.first()
                break
            }
            val ones = numbers.map { it[i] }.count { it == '1' }
            val zeros = numbers.map { it[i] }.count { it == '0' }
            numbers = numbers.filter { it[i] == (if (predicate(ones, zeros)) '1' else '0') }
        }
        if (result == "") {
            result = numbers.first()
        }
        return result
    }

}

