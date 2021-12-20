package pl.touk.dpr.aoc2021

object Day20 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/20/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val imageAlgoritm = lines.first()
        val image = lines.drop(1).map { it.toList() }
//        println(image.map { it.joinToString("") }.joinToString("\n"))
//        println("next")
        var newImage = enhance(imageAlgoritm, image)
//        println(newImage.map { it.joinToString("") }.joinToString("\n"))
//        println("next")
        newImage = enhance(imageAlgoritm, newImage, '#')
//        println(newImage.map { it.joinToString("") }.joinToString("\n"))

        // 5686 is wrong
        // 5631 is wrong
        return newImage.map { it.count { it == '#' } }.sum()
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
                val num = m.map { p -> if (p.first in indicesI && p.second in indicesJ) image[p.first][p.second] else defFill }
                    .map { if (it == '#') 1 else 0 }
                val idx = Integer.parseInt(num.joinToString(""), 2)
                line.add(imageAlgoritm[idx])
            }
            res.add(line)
        }
        return res
    }

    private fun part2(lines: List<String>): Any {
        val imageAlgoritm = lines.first()
        val image = lines.drop(1).map { it.toList() }
        var newImage = image
        var defFill = '.'
        for (i in 1..50) {
            newImage = enhance(imageAlgoritm, newImage, defFill)
            defFill = if (defFill == '#') '.' else '#'
        }
        return newImage.map { it.count { it == '#' } }.sum()

    }
}


