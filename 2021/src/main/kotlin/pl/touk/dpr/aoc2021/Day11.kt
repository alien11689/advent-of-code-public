package pl.touk.dpr.aoc2021

import java.util.Stack

object Day11 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/11/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val numbers = lines.map { it.map { it.toString().toInt() }.toMutableList() }.toMutableList()
//        printBoard(numbers)
        var flashesCount = 0L
        for (i in 1..100) {
            flashesCount += tick1(numbers).size
        }
//        val flashed = tick1(numbers)
//        flashesCount += flashed.size
//        printBoard(numbers)
//        println(flashesCount)
//
//        flashesCount += tick1(numbers).size
//        printBoard(numbers)
//        println(flashesCount)
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

    private fun printBoard(numbers: List<List<Any>>) {
        println(numbers.map { it.joinToString("") }.joinToString("\n"))
        println("=======================")
    }

    private fun part2(lines: List<String>): Any {
        return -1
    }

    data class Point(val x: Int, val y: Int)

    private fun neigh(p: Point): Set<Point> {
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
            .filter { it.x >= 0 && it.x < 10 && it.y >= 0 && it.y < 10 }
            .toSet()
    }
}

