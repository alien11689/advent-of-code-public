package pl.touk.dpr.aoc2020.day03

import pl.touk.dpr.aoc2020.Util

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
//        val input = Util.getFileContent("/03/test1")
        val input = Util.getFileContent("/03/input.txt")
        part1(input)
        part2(input)
    }

    private fun part1(input: String) {
        val lines = input.lines()
                .filter { it.isNotEmpty() }
        val stepX = 3
        val stepY = 1
        val trees = countTrees(lines, stepY, stepX)
        println(trees)
    }

    private fun countTrees(lines: List<String>, stepY: Int, stepX: Int): Long {
        var i = 0
        var j = 0
        var trees = 0L
        while (i < lines.size) {
            trees += if (lines[i][j % lines[0].length] == '#') 1 else 0
            i += stepY
            j += stepX
        }
        return trees
    }

    private fun part2(input: String) {
        val lines = input.lines()
                .filter { it.isNotEmpty() }
        println(countTrees(lines, 1, 1) *
                countTrees(lines, 1, 3) *
                countTrees(lines, 1, 5) *
                countTrees(lines, 1, 7) *
                countTrees(lines, 2, 1)

        )
    }

}

