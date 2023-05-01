package dpr.aoc2021

object Day06 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getLinesFromFile("/06/input.txt")
        println(calculate(lines, 80))
        println(calculate(lines, 256))
    }

    private fun calculate(lines: List<String>, iter: Int): Long {
        var m = mutableMapOf<Int, Long>()
        lines[0].split(",").map { it.toInt() }.forEach {
            m[it] = (m[it] ?: 0) + 1
        }
        for (i in 1..iter) {
            val newM = mutableMapOf<Int, Long>()
            m.forEach { (key, value) ->
                if (key == 0) {
                    newM[6] = (newM[6] ?: 0) + value
                    newM[8] = (newM[8] ?: 0) + value
                } else {
                    newM[key - 1] = (newM[key - 1] ?: 0) + value
                }
            }
            m = newM
        }
        return m.values.sum()
    }
}

