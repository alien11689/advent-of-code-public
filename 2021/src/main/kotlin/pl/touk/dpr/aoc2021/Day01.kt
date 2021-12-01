package pl.touk.dpr.aoc2021

object Day01 {
    @JvmStatic
    fun main(args: Array<String>) {
        val numbers = Util.getNotEmptyLinesFromFile("/01/input.txt")
                .map { it.toInt() }

        println(part1(numbers))
        println(part2(numbers))
    }

    private fun part1(numbers: List<Int>): Int {
        var last = numbers[0]
        var incr = 0
        for (cur in numbers) {
            if (cur > last) {
                ++incr
            }
            last = cur
        }
        return incr
    }

    private fun part2(numbers: List<Int>): Int {
        var cur = 0
        var incr = 0
        var prev = 0
        for (i in numbers.indices) {
            prev = cur
            cur += numbers[i]
            if (i == 0 || i == 1 || i == 2) {
                continue
            }
            cur -= numbers[i - 3]
            if (prev < cur) {
                ++incr
            }
//            println("Prev = $prev vs cur = $cur, incr = $incr")
        }
        return incr //not 1726,1725
    }
}

