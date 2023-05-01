package dpr.eulerproject

import java.math.BigInteger

object Problem0016 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val res = BigInteger.valueOf(2).pow(1000).toString().toList().sumOf { it.toString().toInt() }
        println(res)
    }
}
