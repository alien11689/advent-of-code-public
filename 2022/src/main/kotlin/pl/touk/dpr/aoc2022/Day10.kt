package pl.touk.dpr.aoc2022

object Day10 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/10/input.txt")
        println("Part 1:")
//        println(part1(Util.getNotEmptyLinesFromFile("/10/test1.txt")))
        println(part1(lines))
        println("Part 2:")
//        println(part2(Util.getNotEmptyLinesFromFile("/10/test1.txt")))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val values = getCyclesValues(lines)
        return values.filter { it.key in setOf(20, 60, 100, 140, 180, 220) }
            .map { it.key * it.value }
            .sum()
    }

    private fun getCyclesValues(lines: List<String>): MutableMap<Int, Long> {
        var x = 1L
        var cycle = 1
        val values = mutableMapOf<Int, Long>()
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
        return values
    }

    private fun part2(lines: List<String>): Any {
        val cyclesValues = getCyclesValues(lines)
        val sb = StringBuilder()
        for (i in 1..240) {
            val curValue = cyclesValues[i]!!.toInt()
            if ((i - 1) % 40 in setOf(curValue - 1, curValue, curValue + 1)) {
                sb.append("#")
            } else {
                sb.append(".")
            }
            if (i % 40 == 0) {
                sb.append("\n")
            }
        }
        return sb.trim().toString()
    }
}

