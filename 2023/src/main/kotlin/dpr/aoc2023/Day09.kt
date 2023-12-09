package dpr.aoc2023

object Day09 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/09/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        return lines.sumOf { line ->
            val values = line.split(" ").map { it.toLong() }
            findNextValue(values)
        }
    }

    private fun findNextValue(values: List<Long>): Long {
        if (values.toSet().size == 1) {
            return values[0]
        } else {
            val lowerList = values.windowed(2, 1, false) { it[1] - it[0] }
            val increment = findNextValue(lowerList)
            return values.last() + increment
        }
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

