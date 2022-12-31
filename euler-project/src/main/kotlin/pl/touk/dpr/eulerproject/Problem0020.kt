package pl.touk.dpr.eulerproject

import java.math.BigInteger

object Problem0020 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val res = (2..100L).fold(BigInteger.ONE) { acc, l -> acc * BigInteger.valueOf(l) }
                .toString().sumOf { it.toString().toInt() }
        println(res)
    }
}
