package dpr.aoc2021

import dpr.commons.Util

object Day08 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/08/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    @JvmStatic
    fun part1(lines: List<String>): Int {
        return lines.flatMap { it.split(" | ")[1].split(" ") }
            .count { it.length in setOf(2, 3, 4, 7) }
    }

    @JvmStatic
    fun part2(lines: List<String>): Long {
        return lines.map { it.split(" | ") }.sumOf { deduceNumber(it[0].split(" "), it[1].split(" ")) }
    }

    private fun deduceNumber(clues: List<String>, answers: List<String>): Long {
        val segments = (0..6).map { ('a'..'g').toSet() }.toMutableList()
        segments[2] = clues.first { it.length == 2 }.toSet()
        segments[5] = clues.first { it.length == 2 }.toSet()
        segments[0] = clues.first { it.length == 3 }.toSet() - segments[5]
        segments[1] = clues.first { it.length == 4 }.toSet() - segments[5]
        segments[3] = clues.first { it.length == 4 }.toSet() - segments[5]

        segments[4] = clues.filter { it.length == 5 || it.length == 6 }.flatMap { it.toSet() }
            .toSet() - segments[0] - segments[1] - segments[2] - segments[5] - segments[3]
        segments[6] = clues.filter { it.length == 5 || it.length == 6 }.flatMap { it.toSet() }
            .toSet() - segments[0] - segments[1] - segments[2] - segments[5] - segments[3]
//        val five = clues.filter { it.length == 5 }.map { it.toSet() }.toSet()
        val six = clues.filter { it.length == 6 }.map { it.toSet() }.toSet()
//        println(six)
//        println(five)
        segments[5] = six.filter { !it.containsAll(segments[2]) }.flatten().filter { it in segments[2] }.toSet()
        segments[2] -= segments[5]
        segments[1] = six.filter { !it.containsAll(segments[3]) }.flatten().filter { it in segments[3] }.toSet()
        segments[3] -= segments[1]
        segments[6] = six.filter { !it.containsAll(segments[4]) }.flatten().filter { it in segments[4] }.toSet()
        segments[4] -= segments[6]
        if (segments.any { it.size > 1 }) {
            throw RuntimeException("error $clues")
        }
//        println(segments.map { it.first() })
        return answers.map {
            when (val cur = it.toSet()) {
                segments[0] + segments[2] + segments[5] -> 7
                segments[2] + segments[5] -> 1
                segments[1] + segments[2] + segments[3] + segments[5] -> 4
                segments[0] + segments[1] + segments[2] + segments[4] + segments[5] + segments[6] -> 0
                segments[0] + segments[2] + segments[3] + segments[4] + segments[6] -> 2
                segments[0] + segments[2] + segments[3] + segments[5] + segments[6] -> 3
                segments[0] + segments[1] + segments[3] + segments[5] + segments[6] -> 5
                segments[0] + segments[1] + segments[3] + segments[4] + segments[5] + segments[6] -> 6
                segments[0] + segments[1] + segments[2] + segments[3] + segments[5] + segments[6] -> 9
                segments[0] + segments[1] + segments[2] + segments[3] + segments[4] + segments[5] + segments[6] -> 8
                else -> throw RuntimeException("Unknown $cur")
            }
        }.joinToString(separator = "").toLong()
    }

}

