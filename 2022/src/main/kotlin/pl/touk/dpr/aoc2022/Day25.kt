package pl.touk.dpr.aoc2022

object Day25 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/25/input.txt")
        println("Part 1:")
        println(part1(Util.getNotEmptyLinesFromFile("/25/test1.txt")))
        println(part1(lines))
    }

    private fun part1(lines: List<String>): Any {
        TODO()
    }
}

