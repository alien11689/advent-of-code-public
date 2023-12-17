package dpr.eulerproject

import dpr.commons.Util

object Problem0017 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val res = generateSequence(1) { it + 1 }
            .takeWhile { it <= 1000 }
            .sumOf { countLetters(it) }
        println(res)
    }

    private fun countLetters(value: Int): Long {
        return when {
            value == 0 -> 0
            value == 1000 -> 3 + 8
            value in setOf(1, 2, 6, 10) -> 3
            value in setOf(3, 7, 8) -> 5
            value in setOf(4, 5, 9) -> 4
            value in setOf(11, 12) -> 6
            value in setOf(13, 14, 18, 19) -> 8
            value in setOf(15, 16) -> 7
            value in setOf(17) -> 9
            value >= 100 -> countLetters(value / 100) + 7 + countLetters(value % 100) + if (value % 100 > 0) 3 else 0
            value >= 90 -> 6 + countLetters(value - 90)
            value >= 80 -> 6 + countLetters(value - 80)
            value >= 70 -> 7 + countLetters(value - 70)
            value >= 60 -> 5 + countLetters(value - 60)
            value >= 50 -> 5 + countLetters(value - 50)
            value >= 40 -> 5 + countLetters(value - 40)
            value >= 30 -> 6 + countLetters(value - 30)
            value >= 20 -> 6 + countLetters(value - 20)
            else -> throw RuntimeException("Unknown $value")
        }
    }
}
