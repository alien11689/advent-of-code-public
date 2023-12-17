package dpr.aoc2020

import dpr.commons.Util

object Day02 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/02/input.txt")
        part1(input)
        part2(input)
    }

    private fun part1(input: List<String>) {
        val c = input
            .map { Line.fromLine(it) }
            .count { it.valid() }
        println(c)
    }

    private fun part2(input: List<String>) {
        val c = input
            .map { Line.fromLine(it) }
            .count { it.valid2() }
        println(c)
    }

    data class Line(val min: Int, val max: Int, val c: Char, val s: String) {
        fun valid(): Boolean {
            val count = s.count { it == c }
            return count in min..max
        }

        fun valid2(): Boolean {
            return (s[min - 1] == c || s[max - 1] == c) && s[max - 1] != s[min - 1]
        }

        companion object {
            fun fromLine(line: String): Line {
                val arr = line.split(Regex("[- :]+"))
                return Line(arr[0].toInt(), arr[1].toInt(), arr[2].first(), arr[3])
            }
        }
    }
}

