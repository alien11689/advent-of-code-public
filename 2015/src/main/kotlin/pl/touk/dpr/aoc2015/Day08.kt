package pl.touk.dpr.aoc2015

object Day08 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/08/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Int {
        return input.map { it.length - countStringChars(it) }.sum()
    }

    private fun countStringChars(s: String): Int {
        var i = 1
        var count = 0
        var escaped = false
        while (i < s.length - 1) {
            if (s[i] == '\\') {
                if (escaped) {
                    ++count
                    escaped = false
                } else {
                    escaped = true
                }
                ++i
            } else if (escaped) {
                ++count
                if (s[i] == 'x') {
                    i += 3
                } else {
                    ++i
                }
                escaped = false
            } else {
                ++count
                ++i
            }
        }
        return count
    }

    private fun part2(input: List<String>): Any {
        return input.map { escapedStringLength(it) - it.length }.sum()
    }

    private fun escapedStringLength(s: String): Int {
        return s.fold(2) { acc, c ->
            acc + when (c) {
                '\\' -> 2
                '"' -> 2
                else -> 1
            }
        }
    }
}
