package dpr.eulerproject

import dpr.commons.MathUtil
import dpr.commons.Util

object Problem0010 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val res = generateSequence(2L) { it + 1 }
            .takeWhile { it < 2_000_000 }
            .filter { MathUtil.isPrime(it) }
            .sum()
        println(res)
    }
}
