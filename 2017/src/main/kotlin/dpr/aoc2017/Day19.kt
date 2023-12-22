package dpr.aoc2017

import dpr.commons.Point2D
import dpr.commons.Util

object Day19 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/19/input.txt")
        part1And2(input).forEach { println(it) }
    }

    private fun part1And2(grid: List<String>): Collection<Any> {
        var dir = Pair(0, 1)
        var cur = Point2D(grid[0].indexOf('|'), 0)

        val letters = mutableListOf<Char>()

        var steps = 0
        while (true) {
            val oldCur = cur
            ++steps
            cur = Point2D(cur.x + dir.first, cur.y + dir.second)
            val sign = grid[cur.y][cur.x]
            if (sign == ' ') {
                return listOf(letters.joinToString(""), steps)
            }
            if (sign !in setOf('-', '+', '|')) {
                letters.add(sign)
            }
            if (sign == '+') {
                dir = changeDir(grid, oldCur, cur)
            }
        }
    }

    private fun changeDir(grid: List<String>, oldCur: Point2D, cur: Point2D): Pair<Int, Int> {
        return cur.neighboursCross()
            .filter { it != oldCur }
            .filter { grid[it.y][it.x] != ' ' }
            .map { Pair(it.x - cur.x, it.y - cur.y) }
            .first()
    }
}
