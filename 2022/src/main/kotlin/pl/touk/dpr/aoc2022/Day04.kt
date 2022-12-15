package pl.touk.dpr.aoc2022

object Day04 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/04/input.txt")
        println("Part 1:")
        println(part1(lines))
        println("Part 2:")
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        return lines.count {
            val (l1, r1, l2, r2) = it.split("-", ",").map { it.toInt() }
            val range1 = (l1..r1).toSet()
            val range2 = (l2..r2).toSet()
            range1.containsAll(range2) || range2.containsAll(range1)
        }
    }

    private fun part2(lines: List<String>): Any {
        return lines.count {
            val (l1, r1, l2, r2) = it.split("-", ",").map { it.toInt() }
            val range1 = (l1..r1).toSet()
            val range2 = (l2..r2).toSet()
            range1.intersect(range2).isNotEmpty()
        }
    }
}

