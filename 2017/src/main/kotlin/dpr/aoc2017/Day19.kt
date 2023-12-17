package dpr.aoc2017

import dpr.commons.Util

object Day19 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/19/input.txt")
        part1And2(input).forEach { println(it) }
    }

    private fun part1And2(grid: List<String>): Collection<Any> {
        var dir = Pair(0, 1)
        var cur = Pair(grid[0].indexOf('|'), 0)

        val letters = mutableListOf<Char>()

        var steps = 0
        while (true) {
            val oldCur = cur
            ++steps
            cur = Pair(cur.first + dir.first, cur.second + dir.second)
            val sign = grid[cur.second][cur.first]
            if (sign !in setOf('-', '+', '|')) {
                letters.add(sign)
            }
            if (sign == '+') {
                dir = changeDir(grid, oldCur, cur)
            }
            if (sign == ' ') {
                return listOf(letters.joinToString(""), steps)
            }
        }
    }

    private fun changeDir(grid: List<String>, oldCur: Pair<Int, Int>, cur: Pair<Int, Int>): Pair<Int, Int> {
        return listOf(
            Pair(cur.first + 1, cur.second),
            Pair(cur.first - 1, cur.second),
            Pair(cur.first, cur.second + 1),
            Pair(cur.first, cur.second - 1)
        ).filter { it != oldCur }
            .filter { grid[it.second][it.first] != ' ' }
            .map { Pair(it.first - cur.first, it.second - cur.second) }
            .first()
    }
}
