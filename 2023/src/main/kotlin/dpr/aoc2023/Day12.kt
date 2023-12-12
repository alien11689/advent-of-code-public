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
            val memory = mutableMapOf<Pair<String, List<Int>>, Long>()
            val res = getPossibleOptions(pattern.replace(".", " ").trim(), orderedNumbers, memory)
//            println("$line -> $res")
            res
        }
    }

    private fun getPossibleOptions(pattern: String, orderedNumbers: List<Int>, memory: MutableMap<Pair<String, List<Int>>, Long>): Long {
//        println("Checking '$pattern' against $orderedNumbers")
        val key = pattern to orderedNumbers
        if (key in memory) {
            return memory[key]!!
        }
        if (orderedNumbers.isEmpty()) {
            val res: Long = if (pattern.contains("#")) 0 else 1
            memory[key] = res
            return res
        }
        val patternLength = pattern.length
        val orderedNumbersMinSize = orderedNumbers.sum() + orderedNumbers.size - 1
        if (orderedNumbers.sum() > pattern.count { it != ' ' }) {
            memory[key] = 0
//            println("Shortcut")
            return 0
        }
        if (patternLength < orderedNumbersMinSize) {
            return 0
        }
        var res = 0L
        val regex = if (orderedNumbers.size > 1) "^[?#]{${orderedNumbers[0]}}[? ].*" else "^[?#]{${orderedNumbers[0]}}.*"
//        println("Regex: $regex")
        if (pattern.startsWith(" ")) {
            res += getPossibleOptions(pattern.substring(1), orderedNumbers, memory)
        } else {
            if (pattern.matches(Regex(regex))) {
//            println("Match!!")
                // use first number
                val newPattern = pattern.substring(orderedNumbers[0] + if (orderedNumbers.size > 1) 1 else 0)
                res += getPossibleOptions(newPattern, orderedNumbers.drop(1), memory)
            }
            if (pattern.startsWith("?")) {
                // ? as space
                res += getPossibleOptions(pattern.substring(1), orderedNumbers, memory)
            }
        }
//        println("Result checking '$pattern' against $orderedNumbers -> $res")
//        println(pattern)
//        println(patternLength)
//        println(orderedNumbersMinSize)
        memory[key] = res
        return res
    }

    private fun part2(lines: List<String>): Any {
        return lines.sumOf { line ->
            val (pattern, nums) = line.split(" ")
            val orderedNumbers = nums.split(",").map { it.toInt() }
            val realPattern = listOf(pattern, pattern, pattern, pattern, pattern).joinToString(separator = "?")
            val realNumbers = listOf(orderedNumbers, orderedNumbers, orderedNumbers, orderedNumbers, orderedNumbers).flatten()
            val memory = mutableMapOf<Pair<String, List<Int>>, Long>()
            val res = getPossibleOptions(realPattern.replace(".", " ").trim(), realNumbers, memory)
//            println("$realPattern with $realNumbers -> $res")
            res
        }
//        TODO()
    }
}

