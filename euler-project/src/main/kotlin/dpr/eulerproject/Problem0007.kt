package dpr.eulerproject

import dpr.commons.MathUtil
import dpr.commons.Util

object Problem0007 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val res = generateSequence(2L) { it + 1 }
            .filter { MathUtil.isPrime(it) }
            .drop(10000)
            .first()
        println(res)
    }
}
