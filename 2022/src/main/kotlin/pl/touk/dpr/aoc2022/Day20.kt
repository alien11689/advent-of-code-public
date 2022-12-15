package pl.touk.dpr.aoc2022

object Day20 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/20/input.txt")
        println("Part 1:")
        println(part1(Util.getNotEmptyLinesFromFile("/20/test1.txt")))
        println(part1(lines))
        println("Part 2:")
        println(part2(Util.getNotEmptyLinesFromFile("/20/test1.txt")))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        TODO()
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

