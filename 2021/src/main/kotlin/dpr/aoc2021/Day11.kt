package dpr.aoc2021

import java.util.Stack

object Day11 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/11/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val numbers = lines.map { line -> line.map { it.toString().toInt() }.toMutableList() }.toMutableList()
        var flashesCount = 0L
        for (i in 1..100) {
            flashesCount += tick1(numbers).size
        }
        return flashesCount
    }

    private fun tick1(numbers: MutableList<MutableList<Int>>): MutableSet<Point> {
        val stack = Stack<Point>()
        for (y in numbers.indices) {
            for (x in numbers[y].indices) {
                numbers[y][x] += 1
                if (numbers[y][x] > 9) {
                    val cur = Point(x, y)
                    stack.add(cur)
                }
            }
        }
        val flashed = mutableSetOf<Point>()
        while (!stack.isEmpty()) {
            val cur = stack.pop()
            if (cur in flashed) {
                continue
            }
            if (numbers[cur.y][cur.x] > 9) {
                flashed.add(cur)
                neigh(cur).forEach {
                    numbers[it.y][it.x] += 1
                    stack.add(it)
                }
            }
        }
        flashed.forEach { numbers[it.y][it.x] = 0 }
        return flashed
    }

    private fun part2(lines: List<String>): Any {
        val numbers = lines.map { line -> line.map { it.toString().toInt() }.toMutableList() }.toMutableList()
        var i = 0
        while (true) {
            i += 1
            if (tick1(numbers).size == 100) {
                return i
            }
        }
    }

    data class Point(val x: Int, val y: Int)

    private fun neigh(p: Point): Set<Point> {
        val coordLimit = 0..9
        return setOf(
                Point(p.x, p.y + 1),
                Point(p.x, p.y - 1),
                Point(p.x + 1, p.y - 1),
                Point(p.x + 1, p.y),
                Point(p.x + 1, p.y + 1),
                Point(p.x - 1, p.y + 1),
                Point(p.x - 1, p.y - 1),
                Point(p.x - 1, p.y),
        )
                .filter { it.x in coordLimit && it.y in coordLimit }
                .toSet()
    }
}

