package pl.touk.dpr.aoc2017

object Day04 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/04/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        return input.map {
            it.split(" ")
        }.count {
            it.size == it.toSet().size
        }
    }

    private fun part2(input: List<String>): Any {
        return input.map {
            it.split(" ").toList().map { it.toCharArray().sorted() }
        }.count {
            it.size == it.toSet().size
        }
    }
}