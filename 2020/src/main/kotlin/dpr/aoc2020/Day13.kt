package dpr.aoc2020

import dpr.commons.Util
import java.math.BigInteger

object Day13 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/13/input.txt")
        println(part1(input))
        println(part2(input[1]))
    }

    @JvmStatic
    fun part1(input: List<String>): Long {
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

    @JvmStatic
    fun part2(input: String): BigInteger {
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

    private fun solve(
        base: BigInteger,
        increment: BigInteger,
        keys: List<BigInteger>,
        busToMinut: Map<BigInteger, BigInteger>
    ): BigInteger {
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

