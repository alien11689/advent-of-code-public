package dpr.aoc2022

import dpr.commons.Util

object Day10 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/10/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    @JvmStatic
    fun part1(lines: List<String>): Long {
        val values = getCyclesValues(lines)
        val pivotalIndexes = setOf(20, 60, 100, 140, 180, 220)
        return values.filter { it.key in pivotalIndexes }
            .map { it.key * it.value }
            .sum()
    }

    private fun getCyclesValues(lines: List<String>): MutableMap<Int, Long> {
        var x = 1L
        var cycle = 1
        val values = mutableMapOf<Int, Long>()
        values[cycle] = x
        lines.forEach {
            if (it == "noop") {
                ++cycle
                values[cycle] = x
            } else {
                ++cycle
                values[cycle] = x
                ++cycle
                val toAdd = it.split(" ")[1].toInt()
                x += toAdd
                values[cycle] = x
            }
        }
        return values
    }

    @JvmStatic
    fun part2(lines: List<String>): String {
        val cyclesValues = getCyclesValues(lines)
        val sb = StringBuilder()
        repeat(240) { i ->
            val curValue = cyclesValues[i + 1]!!.toInt()
            val symbol = if (i % 40 in setOf(curValue - 1, curValue, curValue + 1)) "#" else " "
            sb.append(symbol)
            if ((i + 1) % 40 == 0) {
                sb.append("\n")
            }
        }
        return sb.trim().toString()
    }
}

