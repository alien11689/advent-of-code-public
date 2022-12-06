package pl.touk.dpr.aoc2022

object Day06 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/06/input.txt")
        println("Part 1:")
        println(part1(lines))
        println("Part 2:")
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        return getFirstIndexAfterDistinctSequence(lines, 4)
    }

    private fun part2(lines: List<String>): Any {
        return getFirstIndexAfterDistinctSequence(lines, 14)
    }

    private fun getFirstIndexAfterDistinctSequence(lines: List<String>, distinctSize: Int): Int {
        val signal = lines.first().toList()
        var i = 0
        signal.windowed(distinctSize, 1).forEach {
            if (it.toSet().size == distinctSize) {
                return i + distinctSize
            }
            ++i
        }
        return -1
    }
}

