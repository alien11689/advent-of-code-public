package dpr.aoc2021

import dpr.commons.Util

object Day20 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/20/input.txt")
        println(part1And2(lines, 2))
        println(part1And2(lines, 50))
    }

    @JvmStatic
    fun part1And2(lines: List<String>, iterations: Int): Int {
        val imageAlgoritm = lines.first()
        val image = lines.drop(1).map { it.toList() }
        var newImage = image
        var defFill = '.'
        for (i in 1..iterations) {
            newImage = enhance(imageAlgoritm, newImage, defFill)
            // if first char in algorithm is hash and last is dot than infinite background constantly changes
            defFill = if (imageAlgoritm[0] == '#' && imageAlgoritm.last() == '.')
                if (defFill == '#') '.' else '#'
            else defFill
//            println("Iter $i")
//            println(image.map { it.joinToString("") }.joinToString("\n"))
        }
        return newImage.sumOf { row -> row.count { it == '#' } }

    }

    private fun enhance(imageAlgoritm: String, image: List<List<Char>>, defFill: Char = '.'): List<List<Char>> {
        val res = mutableListOf<MutableList<Char>>()
        val indicesI = image.indices
        val indicesJ = image[0].indices
        for (i in (indicesI.minOrNull()!! - 1)..(indicesI.maxOrNull()!! + 1)) {
            val line = mutableListOf<Char>()
            for (j in (indicesJ.minOrNull()!! - 1)..(indicesJ.maxOrNull()!! + 1)) {
                val m = listOf(
                    Pair(i - 1, j - 1), Pair(i - 1, j), Pair(i - 1, j + 1),
                    Pair(i, j - 1), Pair(i, j), Pair(i, j + 1),
                    Pair(i + 1, j - 1), Pair(i + 1, j), Pair(i + 1, j + 1),
                )
                val num =
                    m.map { p -> if (p.first in indicesI && p.second in indicesJ) image[p.first][p.second] else defFill }
                        .map { if (it == '#') 1 else 0 }
                val idx = Integer.parseInt(num.joinToString(""), 2)
                line.add(imageAlgoritm[idx])
            }
            res.add(line)
        }
        return res
    }
}


