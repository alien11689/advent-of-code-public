package dpr.aoc2022

import dpr.commons.Util

object Day08 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/08/input.txt")
//        println("Part 1:")
        println(part1(lines))
//        println("Part 2:")
//        println(part2(Util.getNotEmptyLinesFromFile("/08/test1.txt")))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val grid = lines.map { line -> line.map { it.toString().toInt() } }
        var notVisible = 0
        for (i in grid.indices) {
            if (i == 0 || i == grid.size - 1) {
                continue
            }
            for (j in grid[i].indices) {
                if (j == 0 || j == grid[i].size - 1) {
                    continue
                }
                if (isNotVisible(i, j, grid)) {
                    notVisible++
                }
            }
        }
        return (grid.size * grid[0].size) - notVisible
    }

    private fun isNotVisible(i: Int, j: Int, grid: List<List<Int>>): Boolean {
        val cur = grid[i][j]
        val s1 = (0 until i).map { grid[it][j] }.toSet().any { it >= cur }
        val s2 = ((i + 1) until grid.size).map { grid[it][j] }.toSet().any { it >= cur }
        val s3 = (0 until j).map { grid[i][it] }.toSet().any { it >= cur }
        val s4 = ((j + 1) until grid[i].size).map { grid[i][it] }.toSet().any { it >= cur }
        return s1 && s2 && s3 && s4
    }

    private fun part2(lines: List<String>): Any {
        val grid = lines.map { line -> line.map { it.toString().toInt() } }
        var bestScore = 0L
        for (i in grid.indices) {
            if (i == 0 || i == grid.size - 1) {
                continue
            }
            for (j in grid[i].indices) {
                if (j == 0 || j == grid[i].size - 1) {
                    continue
                }
                val score = getScore(i, j, grid)
                if (score > bestScore) {
                    bestScore = score
                }
            }
        }
        return bestScore
    }

    private fun getScore(i: Int, j: Int, grid: List<List<Int>>): Long {
        val cur = grid[i][j]
        val s1 = calculateSight((0 until i).map { grid[it][j] }.reversed(), cur)
        val s2 = calculateSight(((i + 1) until grid.size).map { grid[it][j] }, cur)
        val s3 = calculateSight((0 until j).map { grid[i][it] }.reversed(), cur)
        val s4 = calculateSight(((j + 1) until grid[i].size).map { grid[i][it] }, cur)
//        println("Tree $i, $j see is [$s1, $s2, $s3, $s4]")
        return s1 * s2 * s3 * s4
    }

    private fun calculateSight(sight1: List<Int>, cur: Int): Long {
        var c = 0L
        for (i in sight1.indices) {
            if (sight1[i] < cur) {
                c++
            } else if (sight1[i] >= cur) {
                c++
                break
            }
        }
        return c
    }
}

