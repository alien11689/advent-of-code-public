package dpr.aoc2019

import dpr.commons.Util
import java.math.BigInteger
import kotlin.math.absoluteValue

object Day22 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/22/input.txt")
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: List<String>, deckSize: Long = 10007L, toFind: Long = 2019L): Long {
        val res = input.fold(buildMap(deckSize)) { m, inp -> apply(inp.split(" "), m) }
        return inOrder(res).toList().find { it.second == toFind }!!.first
    }

    @JvmStatic
    fun part2(input: List<String>): BigInteger {
        val reversedInput = input.reversed()
        var size = 10007L

        fun mod(a: Long, b: Long): Long {
            val r = a % b
            return if (r < 0) r + b else r
        }

        fun applyReversed(instr: List<String>, cur: Long, size: Long): Long {
            if (instr[0] == "cut") { // cut X
                val cut = instr[1].toInt()
                return mod(cur + cut, size)
            }
            if (instr[3] == "stack") { //deal into new stack
                return mod(size - 1 - cur, size)
            }
            if (instr[2] == "increment") { // deal with increment X
                val inc = instr[3].toInt()
//        alternative
//        return (BigInteger.valueOf(inc).modInverse(size) * cur).mod(size)
                var i = 0L
                while (true) {
                    val v = cur + (i * size)
                    if (v % inc == 0L) {
                        return v / inc
                    }
                    ++i
                }
            }
            throw RuntimeException()
            // all operations are linear, so it means that their combination is also linear -> y = ax+b
        }

        fun applyIteration(reversedInput: List<String>, toFind: Long, size: Long): Long {
            return reversedInput.fold(toFind) { cur, inp ->
                applyReversed(inp.split(" "), cur, size)
            }
        }

        var cur = 2020L
//        println("Quick test")

//        println("After iteration \t ${0}: \t $cur")
        for (i in 0..1) {
            val prev = cur
            cur = applyIteration(reversedInput, cur, size)
            mod(cur - prev, size)
//            println("After iteration \t ${i + 1}: \t $cur \t diff $diff ")
        }
        size = 119315717514047
        val times = 101741582076661
        cur = 2020L

//        println("Checking first iter")

        input.fold(buildMap(10007)) { m, inp -> apply(inp.split(" "), m) }
//        println(inOrder(res).toList().find {it.second == 2019L})
        val xx = mutableMapOf<Int, Long>()
        xx[0] = cur

//        println("After iteration \t ${0}: \t $cur")
        for (i in 0..1) {
            cur = applyIteration(reversedInput, cur, size)
            xx[i + 1] = cur
//            println("After iteration \t ${i + 1}: \t $cur")
        }
//        println(xx)

// x2 = (a * x1 + b) % size
// x3 = (a * x2 + b) % size

// a = (x2 -x3)/(x1 -x2) => (x2 - x3) * (x1 - x2)^(-1)
// b = x3 - ax2

        val a =
            (xx[1]!!.toBigInteger() - xx[2]!!.toBigInteger()) * ((xx[0]!!.toBigInteger() - xx[1]!!.toBigInteger())).modInverse(size.toBigInteger()) % size.toBigInteger()
        val b = (xx[1]!!.toBigInteger() - a * xx[0]!!.toBigInteger()) % size.toBigInteger() + size.toBigInteger()
//        println("a = $a")
//        println("b = $b")
//        println("f(2020) = ${(a * xx[0]!!.toBigInteger() + b) % size.toBigInteger()}")
//        println("f(f(2020)) = ${(a * xx[1]!!.toBigInteger() + b) % size.toBigInteger()}")

// y1 = ax + b
// y2 = a(ax + b) + b => a^2 * x + ab + b
// y3 = a(y2) + b => a^3 * x + a^2 * b + ab + b
// https://en.wikipedia.org/wiki/Geometric_series
// yn = a^n * x + sum (k=0->n-1) b*a^n => a^n * x + b(1-a^n)/(1 - a) => a^n * x + b(a^n - 1)/(a - 1) => a^n * x + b(a^n - 1) * (a - 1)^(-1)

// (a + b) mod n => (a mod n + b mod n) mod n
// (a * b) mod n => (a mod n * b mod n) mod n

        val begin = xx[0]!!.toBigInteger() * (a.modPow(times.toBigInteger(), size.toBigInteger()))
//        println(begin)
        val end = b * (a.modPow(times.toBigInteger(), size.toBigInteger()) - BigInteger.ONE) * ((a - BigInteger.ONE).modInverse(size.toBigInteger()))
//        println(end)
        return (begin + end) % size.toBigInteger()
    }

    private fun buildMap(size: Long): Map<Long, Long> {
        return (0L until size).associateWith { it }
    }

    private fun apply(instr: List<String>, m: Map<Long, Long>): Map<Long, Long> {
        if (instr[0] == "cut") { // cut X
            return cut(instr[1].toInt(), m.toMutableMap())
        }
        if (instr[3] == "stack") { //deal into new stack
            return dealIntoNewStack(m)
        }
        if (instr[2] == "increment") { // deal with increment X
            return dealWithIncrement(instr[3].toInt(), m)
        }
        throw RuntimeException()
    }

    private fun dealIntoNewStack(m: Map<Long, Long>): Map<Long, Long> {
        var i = 0L
        return m.toList().sortedBy { -it.first }.associate { (i++) to it.second }
    }

    private fun dealWithIncrement(incr: Int, m: Map<Long, Long>): Map<Long, Long> {
        var i = 0L
        return m.toList().sortedBy { it.first }.associate {
            val now = i
            i = (i + incr) % m.size
            now to it.second
        }
    }

    private fun cut(point: Int, m: MutableMap<Long, Long>): Map<Long, Long> {
        var minIndex = m.keys.minOrNull()!!
        var maxIndex = m.keys.maxOrNull()!!
        if (point > 0) {
            repeat(point) {
                val v = m[minIndex]!!
                m.remove(minIndex)
                ++minIndex
                m[maxIndex + 1] = v
                ++maxIndex
            }
        } else {
            repeat(point.absoluteValue) {
                val v = m[maxIndex]!!
                m.remove(maxIndex)
                --maxIndex
                m[minIndex - 1] = v
                --minIndex
            }
        }
        return m
    }

    private fun inOrder(m: Map<Long, Long>): Map<Long, Long> {
        var i = 0L
        return m.toList().sortedBy { it.first }.associate {
            val now = i
            i = (i + 1) % m.size
            now to it.second
        }
    }
}
