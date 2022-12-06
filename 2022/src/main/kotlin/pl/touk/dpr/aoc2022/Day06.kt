package pl.touk.dpr.aoc2022

object Day06 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/06/input.txt")
        println("Part 1:")
//        println(part1(listOf("mjqjpqmgbljsphdztnvjfqwrcgsmlb")))
        println(part1(lines))
        println("Part 2:")
//        println(part2(listOf("bvwbjplbgvbhsrlpgdmjqwftvncz")))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val signal = lines.first()
        var i = 0
        while (i < signal.length) {
            val subsignal = signal.substring(i, i + 4)
//            println("Checking $i and signal $subsignal")
            if (subsignal.toSet().size == 4) {
                return i + 4
            }
            ++i
        }
        return -1
    }

    private fun part2(lines: List<String>): Any {
        val signal = lines.first().toList()
        var i = 0
        while (i < signal.size) {
//            println("Checking $i of ${signal.size}")
            val subsignal = signal.subList(i, i + 14)
//            println("Checking $i and signal $subsignal")
            if (subsignal.toSet().size == 14) {
                return i + 14
            }
            ++i
        }
        return -1
    }
}

