package dpr.aoc2015

import dpr.commons.Util

object Day05 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/05/input.txt")
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: List<String>): Any {
        return input
            .filter { line -> line.count { it in setOf('a', 'e', 'i', 'o', 'u') } >= 3 }
            .filter { inp -> setOf("ab", "cd", "pq", "xy").none { inp.contains(it) } }
            .count {
                var i = 0
                var res = false
                while (i < it.length - 1) {
                    if (it[i] == it[i + 1]) {
                        res = true
                        break
                    }
                    ++i
                }
                res
            }
    }

    @JvmStatic
    fun part2(input: List<String>): Any {
        return input
            .filter {
                var i = 0
                var res = false
                while (i < it.length - 2) {
                    if (it[i] == it[i + 2]) {
                        res = true
                        break
                    }
                    ++i
                }
                res
            }
            .count {
                var i = 0
                var res = false
                while (i < it.length - 2) {
                    if (it.substring(i + 2).contains(it.substring(i, i + 2))) {
                        res = true
                        break
                    }
                    ++i
                }
                res
            }
    }
}
