package pl.touk.dpr.aoc2015

object Day02 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/02/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        return input
                .map { Box(it.split("x").map { it.toInt() }) }
                .map { it.area() + it.smallestSideArea() }
                .sum()
    }

    private fun part2(input: List<String>): Any {
        return input
                .map { Box(it.split("x").map { it.toInt() }) }
                .map { it.volume() + it.dimensionsToWrap() }
                .sum()
    }

    data class Box(val sides: List<Int>) {
        fun smallestSideArea(): Int = sides.sorted().take(2).fold(1) { acc, i -> acc * i }

        fun area(): Int = 2 * sides[0] * sides[1] + 2 * sides[0] * sides[2] + 2 * sides[1] * sides[2]

        fun volume(): Int = sides.fold(1) { acc, i -> acc * i }

        fun dimensionsToWrap(): Int = 2 * sides.sorted().take(2).fold(0) { acc, i -> acc + i }
    }
}
