package pl.touk.dpr.aoc2017

object Day01 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getFileContent("/01/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        return input.mapIndexed { index, s ->
            val next = input[(index + 1) % input.length]
            if (next == s) s.toString().toInt() else 0
        }.sum()
    }

    private fun part2(input: String): Any {
        return input.mapIndexed { index, s ->
            val next = input[(index + input.length / 2) % input.length]
            if (next == s) s.toString().toInt() else 0
        }.sum()
    }
}