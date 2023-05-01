package dpr.eulerproject

import java.math.BigInteger

object Problem0025 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val res = generateSequence(Triple(1, BigInteger.ZERO, BigInteger.ONE)) {
            Triple(it.first + 1, it.third, it.second + it.third)
        }
                .first { it.third.toString().length >= 1000 }
                .first
        println(res)
    }
}
