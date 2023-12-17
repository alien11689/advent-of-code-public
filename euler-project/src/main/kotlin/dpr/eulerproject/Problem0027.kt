package dpr.eulerproject

import dpr.commons.MathUtil
import dpr.commons.Util

object Problem0027 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val primes = generateSequence(2) { it + 1 }
            .filter { MathUtil.isPrime(it.toLong()) }
            .take(20000)
            .toSet()

        val possibleB = primes.takeWhile { it < 1000 }

        var cur = -1000 to -1000
        var maxConsecutive = 0

        generateSequence(-999) { it + 1 }
            .takeWhile { it < 1000 }
            .forEach { a ->
                possibleB.forEach { b ->
                    val consecutive = consecutivePrimes(a, b, primes)
                    if (consecutive > maxConsecutive) {
                        cur = a to b
                        maxConsecutive = consecutive
//                            println("Leader $cur with $maxConsecutive")
                    }
                }
            }

//        println(cur)
        println(cur.first * cur.second)
    }

    private fun consecutivePrimes(a: Int, b: Int, primes: Set<Int>): Int {
//        println("Checking $a and $b")
        var n = 0
        while (true) {
            ++n
            val res = n * n + a * n + b
            if (res !in primes) {
                return n
            }
        }
    }
}
