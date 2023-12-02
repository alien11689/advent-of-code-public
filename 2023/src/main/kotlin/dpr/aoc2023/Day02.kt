package dpr.aoc2023

object Day02 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/02/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    val limits = mapOf(
        "green" to 13,
        "red" to 12,
        "blue" to 14,
    )

    private fun part1(lines: List<String>): Any {
        return lines.sumOf { line ->
            val (header, rest) = line.split(": ")
            val rounds = rest.split("; ")
            val gameId = header.split(" ")[1].toInt()
            val anyRoundExceedsLimits = rounds.any { round ->
                val m = round.split(Regex("[ ,]+")).chunked(2) { l -> l[1] to l[0].toInt() }.toMap()
                limits.any { (color, limit) -> (m[color] ?: 0) > limit }
            }
            if (anyRoundExceedsLimits) 0 else gameId
        }
    }

    private fun part2(lines: List<String>): Any {
        return lines.sumOf { line ->
            val (_, rest) = line.split(": ")
            val rounds = rest.split("; ")
            val max = mutableMapOf("blue" to 1L, "green" to 1L, "red" to 1L)
            for (round in rounds) {
                val minorResults = round.split(Regex("[ ,]+"))
                var i = 0
                val m = mutableMapOf<String, Long>()
                while (i < minorResults.size) {
                    m[minorResults[i + 1]] = minorResults[i].toLong()
                    i += 2
                }
                setOf("green", "blue", "red").forEach { color ->
                    if ((m[color] ?: 0) > max[color]!!) {
                        max[color] = m[color]!!
                    }
                }
            }
            setOf("green", "blue", "red").fold(1L) { acc, color -> acc * max[color]!! }
        }
    }
}

