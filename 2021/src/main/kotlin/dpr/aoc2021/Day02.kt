package dpr.aoc2021

import dpr.commons.Util

object Day02 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/02/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Int {
        val horizontalAndDepth = lines.fold(Pair(0, 0)) { acc, cur ->
            val parts = cur.split(" ")
            val value = parts[1].toInt()
            when (parts[0]) {
                "forward" -> Pair(acc.first + value, acc.second)
                "down" -> Pair(acc.first, acc.second + value)
                "up" -> Pair(acc.first, acc.second - value)
                else -> throw RuntimeException()
            }
        }
        return horizontalAndDepth.first * horizontalAndDepth.second
    }

    private fun part2(lines: List<String>): Long {
        val horizontalAndDepth = lines.fold(Triple(0L, 0L, 0L)) { acc, cur ->
            val parts = cur.split(" ")
            val value = parts[1].toInt()
            when (parts[0]) {
                "forward" -> Triple(acc.first + value, acc.second + acc.third * value, acc.third)
                "down" -> Triple(acc.first, acc.second, acc.third + value)
                "up" -> Triple(acc.first, acc.second, acc.third - value)
                else -> throw RuntimeException()
            }
        }
        return horizontalAndDepth.first * horizontalAndDepth.second
    }

}

