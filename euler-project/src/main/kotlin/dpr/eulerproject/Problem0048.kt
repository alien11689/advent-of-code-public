package dpr.eulerproject

import dpr.commons.Util
import java.math.BigInteger

object Problem0048 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lastDigits = BigInteger.valueOf(1_000_000_000_0L)
        val res = generateSequence(1L) { it + 1 }
            .takeWhile { it <= 1000 }
            .sumOf { BigInteger.valueOf(it).modPow(BigInteger.valueOf(it), lastDigits) }
            .mod(lastDigits)
        println(res)
    }
}
