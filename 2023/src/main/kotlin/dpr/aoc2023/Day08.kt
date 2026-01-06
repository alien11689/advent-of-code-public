package dpr.aoc2023

import dpr.commons.MathUtil
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
        val directions = lines[0]
        val mapping = readMapping(lines)
        var cur = "AAA"
        var steps = 0
        while (cur != "ZZZ") {
            val curMapping = mapping[cur]!!
            cur = if (directions[steps % directions.length] == 'L') curMapping.first else curMapping.second
            ++steps
        }
        return steps
    }

    @JvmStatic
    fun part2(lines: List<String>): Long {
        val directions = lines[0]
        val mapping = readMapping(lines)
        val startingPoints = mapping.filter { it.key.endsWith("A") }.keys
        return startingPoints.map {
            var steps = 0
            var cur = it
            while (steps < 100000) {
                if (cur.endsWith("Z")) {
                    break
                }
                val curMapping = mapping[cur]!!
                cur = if (directions[steps % directions.length] == 'L') curMapping.first else curMapping.second
                ++steps
            }
            steps
        }

            .fold(1L) { acc, cur -> MathUtil.lowestCommonMultiple(acc, cur.toLong()) }
        // 27011809034838424525673297 is too high
    }

    private fun readMapping(lines: List<String>): MutableMap<String, Pair<String, String>> {
        val mapping = mutableMapOf<String, Pair<String, String>>()
        lines.drop(1).forEach {
            val split = it.split(Regex("[ = (,)]+"))
            mapping[split[0]] = split[1] to split[2]
        }
        return mapping
    }
}

