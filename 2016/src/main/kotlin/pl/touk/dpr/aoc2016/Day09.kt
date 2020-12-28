package pl.touk.dpr.aoc2016

object Day09 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getFileContent("/09/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        var i = 0
        var count = 0
        while (i < input.length) {
            if (input[i] == '(') {
                val rule = input.substring(i + 1, input.indexOf(')', i + 1))
                val parts = rule.split(Regex("x")).map { it.toInt() }
                val length = parts[0]
                val times = parts[1]
                count += length * times
                i += rule.length + 2 + length
            } else {
                ++i
                ++count
            }
        }
        return count
    }

    private fun part2(input: String): Long {
        var i = 0
        var count = 0L
        while (i < input.length) {
            if (input[i] == '(') {
                val rule = input.substring(i + 1, input.indexOf(')', i + 1))
                val parts = rule.split(Regex("x")).map { it.toInt() }
                val length = parts[0]
                val times = parts[1]
                i += rule.length + 2
                count +=  times * part2(input.substring(i, i + length))
                i += length
            } else {
                ++i
                ++count
            }
        }
        return count
    }
}