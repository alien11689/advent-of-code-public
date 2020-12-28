package pl.touk.dpr.aoc2016

object Day12 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/12/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        return Assembunny(input).run().first["a"]!!
    }

    private fun part2(input: List<String>): Any {
        return Assembunny(input).run(mapOf(Pair("c", 1))).first["a"]!!
    }
}