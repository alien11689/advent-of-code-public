package pl.touk.dpr.aoc2020.day02

import pl.touk.dpr.aoc2020.Util

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getFileContent("/02/input.txt")
        part1(input)
        part2(input)
    }

    private fun part1(input: String) {
        val c = input.lines()
                .filter { it.isNotEmpty() }
                .map { Line.fromLine(it) }
                .count { it.valid() }
        println(c)
    }

    private fun part2(input: String) {
        val c = input.lines()
                .filter { it.isNotEmpty() }
                .map { Line.fromLine(it) }
                .count { it.valid2() }
        println(c)
    }

    data class Line(val min: Int, val max: Int, val c: Char, val s: String) {
        fun valid():Boolean {
            val count = s.count { it == c }
            return count in min..max
        }

        fun valid2():Boolean {
            return (s[min-1] == c || s[max-1] == c) && s[max-1] != s[min-1]
        }

        companion object {
            fun fromLine(line: String): Line {
                val arr = line.split(Regex("[- :]+"))
                return Line(arr[0].toInt(), arr[1].toInt(), arr[2].first(), arr[3])
            }
        }
    }
}

