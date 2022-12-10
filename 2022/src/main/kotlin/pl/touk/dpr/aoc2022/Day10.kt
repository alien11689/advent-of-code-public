package pl.touk.dpr.aoc2022

object Day10 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/10/input.txt")
        println("Part 1:")
        println(part1(Util.getNotEmptyLinesFromFile("/10/test1.txt")))
        println(part1(lines))
        println("Part 2:")
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        var x = 1L
        var cycle = 1
        var values = mutableMapOf<Int, Long>()
        values[cycle] = x
        lines.forEach {
            if (it == "noop") {
                ++cycle
                values[cycle] = x
            } else {
                ++cycle
                values[cycle] = x
                ++cycle
                val toAdd = it.split(" ")[1].toInt()
                x += toAdd
                values[cycle] = x
            }
        }
        return values.filter { it.key in setOf(20, 60, 100, 140, 180, 220) }
            .map { it.key * it.value }
            .sum()
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

