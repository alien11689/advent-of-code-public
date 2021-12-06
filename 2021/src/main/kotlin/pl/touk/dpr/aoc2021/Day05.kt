package pl.touk.dpr.aoc2021

import kotlin.math.absoluteValue
import kotlin.math.max

object Day05 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/05/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Int {
        val m = mutableMapOf<Pair<Int, Int>, Int>()
        lines.map { it.split(' ', '>', ',', '-').filter { it.isNotEmpty() }.map { it.toInt() } }
            .filter { it[0] == it[2] || it[1] == it[3] }
            .forEach {
                for (i in listOf(it[0], it[2]).minOrNull()!!..listOf(it[0], it[2]).maxOrNull()!!) {
                    for (j in listOf(it[1], it[3]).minOrNull()!!..listOf(it[1], it[3]).maxOrNull()!!) {
                        val p = Pair(i, j)
                        m[p] = m.getOrDefault(p, 0) + 1
                    }
                }
            }
        return m.values.count { it > 1 }
    }

    private fun part2(lines: List<String>): Int {
        val m = mutableMapOf<Pair<Int, Int>, Int>()
        val coords = lines.map { it.split(' ', '>', ',', '-').filter { it.isNotEmpty() }.map { it.toInt() } }
        coords.forEach {
            val stepX = if (it[0] < it[2]) 1 else if (it[0] > it[2]) -1 else 0
            val stepY = if (it[1] < it[3]) 1 else if (it[1] > it[3]) -1 else 0
            val times = max((it[0] - it[2]).absoluteValue, (it[1] - it[3]).absoluteValue)
            val initP = Pair(it[0], it[1])
            for (i in 0..times) {
                val p = Pair(initP.first + stepX * i, initP.second + stepY * i)
                m[p] = m.getOrDefault(p, 0) + 1
            }
        }
        return m.values.count { it > 1 }
    }
}

