package dpr.aoc2016

object Day04 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/04/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        return input.map { line -> Line.from(line.split(Regex("[-\\[\\]]+")).filter { it.isNotEmpty() }) }
                .filter { it.isValid() }
                .sumOf { it.id }
    }

    private fun part2(input: List<String>): Any {
        return input.map { line -> Line.from(line.split(Regex("[-\\[\\]]+")).filter { it.isNotEmpty() }) }
                .first { it.decrypt().contains("north") }
                .id
    }

    data class Line(val words: List<String>, val id: Int, val checksum: String) {
        companion object {
            fun from(parts: List<String>): Line {
                return Line(parts.take(parts.size - 2), parts[parts.size - 2].toInt(), parts[parts.size - 1])
            }
        }

        fun isValid(): Boolean {
            return words
                    .asSequence()
                    .flatMap { it.toCharArray().toList() }
                    .fold(mapOf<Char, Int>()) { acc, c ->
                        acc + Pair(c, if (acc.containsKey(c)) acc[c]!! + 1 else 1)
                    }
                    .toList()
                    .sortedWith { o1, o2 ->
                        if (o1.second == o2.second) o1.first - o2.first else o2.second - o1.second
                    }
                    .take(checksum.length)
                    .map { it.first }
                    .joinToString(separator = "") == checksum

        }

        fun decrypt(): String {
            return words.joinToString("").map {
                ((it.code - 97 + id) % 26 + 97).toChar()
            }.joinToString("")
        }
    }
}
