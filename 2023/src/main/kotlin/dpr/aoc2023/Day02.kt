package dpr.aoc2023

object Day02 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/02/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        return lines.sumOf { line ->
            val (header, rest) = line.split(": ")
            val rounds = rest.split("; ")
            var res = header.split(" ")[1].toInt()
            for (round in rounds) {
                val minorResults = round.split(Regex("[ ,]+"))
                var i = 0
                val m = mutableMapOf<String, Int>()
                while (i < minorResults.size) {
                    m[minorResults[i + 1]] = minorResults[i].toInt()
                    i += 2
                }
                if ((m["green"] ?: 0) > 13 || (m["red"] ?: 0) > 12 || (m["blue"] ?: 0) > 14) {
                    res = 0
                    break
                }
            }
            res
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

