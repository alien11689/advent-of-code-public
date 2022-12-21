package pl.touk.dpr.aoc2018

object Day01 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/01/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        return input.sumBy { it.toInt() }
    }

    private fun part2(input: List<String>): Any {
        val numbers = input.map { it.toInt() }
        val m = mutableSetOf<Int>()
        var start = 0
        while (true) {
            start = numbers.fold(start) { acc, cur ->
                val next = acc + cur
                if (next in m) {
                    return next
                }
                m.add(next)
                next
            }
        }
    }
}
