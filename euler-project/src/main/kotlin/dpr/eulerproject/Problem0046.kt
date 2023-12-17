package dpr.eulerproject

import dpr.commons.MathUtil
import dpr.commons.Util
import kotlin.math.sqrt

object Problem0046 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val primes = mutableListOf<Long>()
        var i = 1L
        while (i < 100_000_000L) {
            i += 2
            if (MathUtil.isPrime(i)) {
                primes.add(i)
//                println("Found prime $i")
                continue
            }
            val match = primes.none {
//                println("   Checking with $it as prime")
                val x = (i - it) / 2
                val sq = sqrt(x.toDouble()).toLong()
                sq * sq == x
            }
            if (match) {
                println(i)
                break
            }
        }

    }
}
