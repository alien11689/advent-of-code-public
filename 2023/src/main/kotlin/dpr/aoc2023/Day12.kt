package dpr.aoc2023

object Day12 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/12/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/12/test1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        return lines.sumOf { line ->
            val (pattern, nums) = line.split(" ")
            val orderedNumbers = nums.split(",").map { it.toInt() }
            val res = getPossibleOptions(pattern.replace(".", " ").trim(), orderedNumbers)
            println("$line -> $res")
            res
        }
    }

    private fun getPossibleOptions(pattern: String, orderedNumbers: List<Int>): Int {
        println("Checking '$pattern' against $orderedNumbers")
        if (orderedNumbers.isEmpty()) {
            return if (pattern.contains("#")) 0 else 1
        }
        val patternLength = pattern.length
        val orderedNumbersMinSize = orderedNumbers.sum() + orderedNumbers.size - 1
        if (patternLength < orderedNumbersMinSize) {
            return 0
        }
        var res = 0
        val regex = if (orderedNumbers.size > 1) "^[?#]{${orderedNumbers[0]}}[? ].*" else "^[?#]{${orderedNumbers[0]}}.*"
        println("Regex: $regex")
        if (pattern.matches(Regex(regex))) {
//            println("Match!!")
            // use first number
            val newPattern = pattern.substring(orderedNumbers[0] + if (orderedNumbers.size > 1) 1 else 0)
            res += getPossibleOptions(newPattern, orderedNumbers.drop(1))
        }
        if (pattern.startsWith(" ") || pattern.startsWith("?")) {
            res += getPossibleOptions(pattern.substring(1), orderedNumbers)
        }
        println("Result checking '$pattern' against $orderedNumbers -> $res")
//        println(pattern)
//        println(patternLength)
//        println(orderedNumbersMinSize)
        return res
        TODO("Not yet implemented")
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

