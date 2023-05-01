package dpr.aoc2017

object Day17 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        println(part1())
        println(part2())
    }

    private fun part1(): Any {
        val input = 344

        val buf = mutableListOf(0)
        var cur = 0

        (1..2017).forEach { i ->
            var curInput = input % buf.size
            while (curInput > 0) {
                cur = (cur + 1) % buf.size
                --curInput
            }
            buf.add(++cur, i)
        }
        return buf[cur + 1]
    }

    private fun part2(): Any {
        val input = 344
        val amount = 50000000

        var size = 1
        var cur = 0
        var nextToZero = -1

        (1..amount).forEach { i ->
            val curInput = input % size
            cur = (cur + curInput) % size
            ++cur
            ++size
            if (cur == 1) {
                nextToZero = i
            }
        }
        return nextToZero
    }
}
