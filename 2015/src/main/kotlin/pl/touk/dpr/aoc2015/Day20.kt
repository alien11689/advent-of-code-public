package pl.touk.dpr.aoc2015

object Day20 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = 29000000
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: Int): Any {
        var cur = 1
        while (true) {
            val res = divisors(cur).map { it * 10 }.sum()
            if (res >= input) {
                return cur
            }
            ++cur
        }
    }

    private fun divisors(n: Int): Set<Int> {
        val ms = mutableSetOf(1, n)
        var i = 2
        val limit = Math.sqrt(n.toDouble())
        while (i < limit) {
            if (n % i == 0) {
                ms.add(i)
                ms.add(n / i)
            }
            ++i
        }
        return ms
    }

    private fun part2(input: Int): Any {
        var cur = 1
        while (true) {
            val res = divisors(cur).filter { it * 50 >= cur }.map { it * 11 }.sum()
            if (res >= input) {
                return cur
            }
            ++cur
        }
    }
}