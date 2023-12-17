package dpr.aoc2018

import dpr.commons.Util

object Day02 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/02/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        var count2 = 0
        var count3 = 0

        input.forEach { line ->
            val m = mutableMapOf<Char, Int>()
            line.forEach { letter ->
                m[letter] = m.getOrDefault(letter, 0) + 1
            }
            count2 += if (2 in m.values) 1 else 0
            count3 += if (3 in m.values) 1 else 0

        }

        return count2 * count3
    }

    private fun part2(lines: List<String>): Any {
        (0 until (lines.size - 1)).forEach { i ->
            val a = lines[i]
            ((i + 1) until (lines.size)).forEach { j ->
                val b = lines[j]
                var diff = 0
                for (k in a.indices) {
                    if (a[k] != b[k]) {
                        ++diff
                        if (diff > 1) {
                            break
                        }
                    }
                }
                if (diff == 1) {
                    return a.indices.map<Int, Any> { if (a[it] == b[it]) a[it] else "" }.joinToString("")
                }
            }
        }
        throw RuntimeException()
    }
}
