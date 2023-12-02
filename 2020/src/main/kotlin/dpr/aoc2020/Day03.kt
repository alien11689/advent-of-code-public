package dpr.aoc2020

object Day03 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
//        val input = Util.getFileContent("/03/test1")
        val input = Util.getNotEmptyLinesFromFile("/03/input.txt")
        part1(input)
        part2(input)
    }

    private fun part1(input: List<String>) {
        val stepX = 3
        val stepY = 1
        val trees = countTrees(input, stepY, stepX)
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

    private fun part2(input: List<String>) {
        println(countTrees(input, 1, 1) *
                countTrees(input, 1, 3) *
                countTrees(input, 1, 5) *
                countTrees(input, 1, 7) *
                countTrees(input, 2, 1)

        )
    }

}
