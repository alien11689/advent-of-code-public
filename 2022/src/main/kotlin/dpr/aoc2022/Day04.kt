package dpr.aoc2022

import dpr.commons.Util

object Day04 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/04/input.txt")
//        println("Part 1:")
        println(part1(lines))
//        println("Part 2:")
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        return lines.count { line ->
            val (range1, range2) = readRanges(line)
            if (range1.size > range2.size) range1.containsAll(range2) else range2.containsAll(range1)
        }
    }

    private fun part2(lines: List<String>): Any {
        return lines.count { line ->
            val (range1, range2) = readRanges(line)
            range1.intersect(range2).isNotEmpty()
        }
    }

    private fun readRanges(line: String): Pair<Set<Int>, Set<Int>> {
        val (l1, r1, l2, r2) = line.split("-", ",").map { it.toInt() }
        val range1 = (l1..r1).toSet()
        val range2 = (l2..r2).toSet()
        return Pair(range1, range2)
    }
}

