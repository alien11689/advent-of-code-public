package pl.touk.dpr.aoc2022

object Day03 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/03/input.txt")
        println("Part 1:")
        println(part1(lines))
        println("Part 2:")
        println(part2(lines))
    }

    val priorities = ('a'..'z') + ('A'..'Z')

    private fun part1(lines: List<String>): Any {
        return lines.map {
            val char = it.chunked(it.length / 2)
                .map { it.toSet() }
                .reduce { a, b -> a.intersect(b) }
                .first()
            priorities.indexOf(char) + 1L
        }.sum()
    }

    private fun part2(lines: List<String>): Any {
        return lines.chunked(3).map {
            val char = it.map { it.toSet() }
                .reduce { a, b -> a.intersect(b) }
                .first()
            priorities.indexOf(char) + 1L
        }.sum()
    }
}

