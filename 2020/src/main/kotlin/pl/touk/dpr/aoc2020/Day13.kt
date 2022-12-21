package pl.touk.dpr.aoc2020

import java.math.BigInteger

object Day13 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
//        val input = Util.getNotEmptyLinesFromFile("/12/test.txt")
        val input = Util.getNotEmptyLinesFromFile("/13/input.txt")
        println(part1(input))
//        Util.test(part2("7,13,x,x,59,x,31,19"), 1068781.toBigInteger())
//        Util.test(part2("17,x,13,19"), 3417.toBigInteger())
//        Util.test(part2("67,7,59,61"), 754018.toBigInteger())
//        Util.test(part2("67,x,7,59,61"), 779210.toBigInteger())
//        Util.test(part2("67,7,x,59,61"), 1261476.toBigInteger())
//        Util.test(part2("1789,37,47,1889"), 1202161486.toBigInteger())
        println(part2(input[1]))
    }

    private fun part1(input: List<String>): Any {
        val timestamp = input[0].toLong()
        val buses = input[1].split(',').filter { it != "x" }.map { it.toLong() }
        var i = 0
        while (true) {
            val ts = timestamp + i
            val bus = buses.find { ts % it == 0L }
            if (bus != null) {
                return i * bus
            }
            ++i
        }
    }

    private fun part2(input: String): BigInteger {
        val buses = input.split(",").toList()
        val busToMinut = mutableMapOf<BigInteger, BigInteger>()
        buses.forEachIndexed { index, s ->
            if (s != "x") {
                busToMinut[s.toBigInteger()] = index.toBigInteger()
            }
        }
        val keys = busToMinut.keys
        var n = 1
        var res = BigInteger.ZERO
        while (n <= keys.size) {
            // IMPORTANT: bus numbers are prime numbers
            // I look for nums divided by first key
            // increment is product of previous checked keys
            res = solve(res, keys.take(n - 1).fold(BigInteger.ONE) { acc, l -> acc * l }, keys.take(n), busToMinut)
//            println("res: $res")
            ++n
        }
        return res
    }

    private fun solve(base: BigInteger, increment: BigInteger, keys: List<BigInteger>, busToMinut: Map<BigInteger, BigInteger>): BigInteger {
        var cur: BigInteger = base
        while (true) {
            if (busToMinut.filter { it.key in keys }.all { entry ->
                        (cur + entry.value) % entry.key == BigInteger.ZERO
                    }) {
                return cur
            }
            cur += increment
        }
    }
}

