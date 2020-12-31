package pl.touk.dpr.aoc2019

object Day01 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/01/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        return input.fold(0) { a, v ->
            a + calculateFuel(v.toInt())
        }
    }

    private fun part2(input: List<String>): Any {
        var sum = 0
        input.forEach { vv ->
            var v = vv.toInt()
            while (v > 0) {
                val newV = calculateFuel(v)
                if (newV > 0) {
                    sum += newV
                }
                v = newV
            }
        }
        return sum
    }

    fun calculateFuel(value: Int): Int {
        return value / 3 - 2
    }
}