package dpr.aoc2023

object Day15 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/15/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/15/test1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        // 467627 is too low
        return lines.sumOf { line ->
            line.split(",").sumOf { hash(it) }
        }
    }

    private fun hash(s: String): Long {
        return s.fold(0L) { acc, c -> (acc + c.code) * 17 % 256 }
//            .also { println("For $s -> $it") }
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

