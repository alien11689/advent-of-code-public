package dpr.aoc2023

object Day01 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/01/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        return lines.sumOf { line ->
            val digits = line.filter { it.isDigit() }
            Integer.parseInt("${digits[0]}${digits[digits.length - 1]}")
        }
    }

    private fun part2(lines: List<String>): Any {
        val mapping = mapOf(
            "one" to "1",
            "two" to "2",
            "three" to "3",
            "four" to "4",
            "five" to "5",
            "six" to "6",
            "seven" to "7",
            "eight" to "8",
            "nine" to "9",
            "zero" to "0",
        )
        val startingRegexToDigit = mapping.map { (word, digit) -> Regex("^${word}") to digit }
        val endingRegexToDigit = mapping.map { (word, digit) -> Regex("${word}$") to digit }
        return lines.sumOf { line ->
            val firstDigit: Char = getFirstDigit(line, startingRegexToDigit)
            val lastDigit: Char = getLastDigit(line, endingRegexToDigit)
            Integer.parseInt("${firstDigit}${lastDigit}")
        }
    }

    private fun getFirstDigit(line: String, startingRegexToDigit: List<Pair<Regex, String>>): Char {
        var curLine = line
        while (true) {
            curLine = startingRegexToDigit.fold(curLine) { l, (w, d) -> l.replace(w, d) }
            if (curLine.first().isDigit()) {
                return curLine.first()
            } else {
                curLine = curLine.substring(1)
            }
        }
    }

    private fun getLastDigit(line: String, endingRegexToDigit: List<Pair<Regex, String>>): Char {
        var curLine = line
        while (true) {
            curLine = endingRegexToDigit.fold(curLine) { l, (w, d) -> l.replace(w, d) }
            if (curLine.last().isDigit()) {
                return curLine.last()
            } else {
                curLine = curLine.substring(0, curLine.length - 1)
            }
        }
    }
}

