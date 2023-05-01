package dpr.aoc2017

object Day09 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/09/input.txt").trim()
        part1And2(input).forEach { println(it) }
    }

    private fun part1And2(input: String): Collection<Any> {
        var score = 0
        var sum = 0
        var garbage = false
        var garbageAmount = 0
        var i = 0
        while (i < input.length) {
            when (input[i]) {
                '!' -> ++i
                '<' -> if (!garbage) garbage = true else ++garbageAmount
                '{' ->
                    if (!garbage) {
                        ++score
                        sum += score
                    } else {
                        ++garbageAmount
                    }
                '>' -> garbage = false
                '}' -> if (!garbage) --score else ++garbageAmount
                else -> if (garbage) garbageAmount++
            }
            ++i
        }
        return listOf(sum, garbageAmount)
    }
}
