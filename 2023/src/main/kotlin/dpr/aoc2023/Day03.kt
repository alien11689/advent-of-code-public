package dpr.aoc2023

import dpr.commons.Point2D
import dpr.commons.Util

object Day03 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/03/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    @JvmStatic
    fun part1(lines: List<String>): Int {
        val (signs, numbers) = parseMap(lines)
        val adjacentPoints = signs.map { it.second }.flatMap { p -> p.adjacentPoints() }.toSet()
        return numbers.filter { it.second.any { p -> p in adjacentPoints } }.sumOf { it.first }
    }

    @JvmStatic
    fun part2(lines: List<String>): Long {
        val (signs, numbers) = parseMap(lines)
        return signs.filter { it.first == '*' }.map { it.second }.sumOf { p ->
            val adjacentPoints = p.adjacentPoints()
            val nums = numbers.filter { it.second.any { p -> p in adjacentPoints } }.map { it.first }
            if (nums.size < 2) 0L else nums.fold(1L) { acc, cur -> acc * cur }
        }
    }

    private fun parseMap(lines: List<String>): Pair<MutableList<Pair<Char, Point2D>>, MutableList<Pair<Int, Set<Point2D>>>> {
        val signs = mutableListOf<Pair<Char, Point2D>>()
        val numbers = mutableListOf<Pair<Int, Set<Point2D>>>()
        lines.forEachIndexed { curY, line ->
            var curNumber = 0
            var curPoints = mutableSetOf<Point2D>()
            line.forEachIndexed { curX, c ->
                when {
                    c == '.' -> {
                        if (curNumber != 0) {
                            numbers.add(curNumber to curPoints)
                            curNumber = 0
                            curPoints = mutableSetOf()
                        }
                    }

                    c.isDigit() -> {
                        curNumber = curNumber * 10 + c.digitToInt()
                        curPoints.add(Point2D(curX, curY))
                    }

                    else -> {
                        signs.add(c to Point2D(curX, curY))
                        if (curNumber != 0) {
                            numbers.add(curNumber to curPoints)
                            curNumber = 0
                            curPoints = mutableSetOf()
                        }
                    }
                }
            }
            if (curNumber != 0) {
                numbers.add(curNumber to curPoints)
                curNumber = 0
                curPoints = mutableSetOf()
            }
        }
        return Pair(signs, numbers)
    }
}

