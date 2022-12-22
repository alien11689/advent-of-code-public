package pl.touk.dpr.aoc2016

object Day06 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/06/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        return (0 until input[0].length).map { col ->
            input.map { it[col] }
                    .groupingBy { it }
                    .eachCount()
                    .maxByOrNull { it.value }!!
                    .key
        }.joinToString("")
    }

    private fun part2(input: List<String>): Any {
        return (0 until input[0].length).map { col ->
            input.map { it[col] }
                    .groupingBy { it }
                    .eachCount()
                    .minByOrNull { it.value }!!
                    .key
        }.joinToString("")
    }
}
