package dpr.aoc2023

import dpr.commons.Util
import kotlin.math.pow

object Day04 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/04/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    @JvmStatic
    fun part1(lines: List<String>): Long {
        return lines.sumOf { line ->
            val (winning, my) = line.split(":")[1].split("|").map { it.trim().split(Regex("\\s+")) }
            val win = my.count { it in winning }
            if (win <= 0) 0 else 2.toDouble().pow(win - 1).toLong()
        }
    }

    @JvmStatic
    fun part2(lines: List<String>): Long {
        val cards = lines.indices.associateWith { 1L }.toMutableMap()
        lines.forEachIndexed { i, line ->
            val win = calculateNumberOfWinningLots(line)
            if (win >= 0) {
                val curCard = cards[i]!!
                for (nextCard in 1..win) {
                    cards[i + nextCard] = cards[i + nextCard]!! + curCard
                }
            }
        }
        return cards.values.sum()
    }

    private fun calculateNumberOfWinningLots(line: String): Int {
        val (winning, my) = line.split(":")[1].split("|").map { it.trim().split(Regex("\\s+")) }
        return my.count { it in winning }
    }
}

