package pl.touk.dpr.aoc2021

object Day08 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/08/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Int {
        return lines.flatMap { it.split(" | ")[1].split(" ") }
            .count { it.length in setOf(2, 3, 4, 7) }
    }

    private fun part2(lines: List<String>): Long {
        return lines.map { it.split(" | ") }.map { deduceNumber(it[0].split(" "), it[1].split(" ")) }.sum()
    }

    private fun deduceNumber(clues: List<String>, answers: List<String>): Long {
        val segments = (0..6).map { ('a'..'g').toSet() }.toMutableList()
        segments[2] = clues.filter { it.length == 2 }.first().toSet()
        segments[5] = clues.filter { it.length == 2 }.first().toSet()
        segments[0] = clues.filter { it.length == 3 }.first().toSet() - segments[5]
        segments[1] = clues.filter { it.length == 4 }.first().toSet() - segments[5]
        segments[3] = clues.filter { it.length == 4 }.first().toSet() - segments[5]

        segments[4] = clues.filter { it.length == 5 || it.length == 6 }.flatMap { it.toSet() }.toSet() - segments[0] - segments[1] - segments[2] - segments[5] - segments[3]
        segments[6] = clues.filter { it.length == 5 || it.length == 6 }.flatMap { it.toSet() }.toSet() - segments[0] - segments[1] - segments[2] - segments[5] - segments[3]
        val six = clues.filter { it.length == 6 }.map { it.toSet() }.toSet()
        val five = clues.filter { it.length == 5 }.map { it.toSet() }.toSet()
//        println(six)
//        println(five)
        segments[5] = six.filter { !it.containsAll(segments[2]) }.flatten().filter { it in segments[2] }.toSet()
        segments[2] -= segments[5]
        segments[1] = six.filter { !it.containsAll(segments[3]) }.flatten().filter { it in segments[3] }.toSet()
        segments[3] -= segments[1]
        segments[6] = six.filter { !it.containsAll(segments[4]) }.flatten().filter { it in segments[4] }.toSet()
        segments[4] -= segments[6]
        if (segments.any { it.size > 1 }) {
            throw RuntimeException("error $clues")
        }
//        println(segments.map { it.first() })
        return answers.map {
            val cur = it.toSet()
            when {
                cur == segments[0] + segments[2] + segments[5] -> 7
                cur == segments[2] + segments[5] -> 1
                cur == segments[1] + segments[2] + segments[3] + segments[5] -> 4
                cur == segments[0] + segments[1] + segments[2] + segments[4] + segments[5] + segments[6] -> 0
                cur == segments[0] + segments[2] + segments[3] + segments[4] + segments[6] -> 2
                cur == segments[0] + segments[2] + segments[3] + segments[5] + segments[6] -> 3
                cur == segments[0] + segments[1] + segments[3] + segments[5] + segments[6] -> 5
                cur == segments[0] + segments[1] + segments[3] + segments[4] + segments[5] + segments[6] -> 6
                cur == segments[0] + segments[1] + segments[2] + segments[3] + segments[5] + segments[6] -> 9
                cur == segments[0] + segments[1] + segments[2] + segments[3] + segments[4] + segments[5] + segments[6] -> 8
                else -> throw RuntimeException("Unknown $cur")
            }
        }.joinToString(separator = "").toLong()
    }

}

