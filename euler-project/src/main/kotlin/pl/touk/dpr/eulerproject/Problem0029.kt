package pl.touk.dpr.eulerproject

import java.lang.StringBuilder
import java.math.BigInteger

object Problem0029 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val s = mutableSetOf<BigInteger>()
        (2..100L).forEach { a ->
            (2..100).forEach{b->
                s.add(BigInteger.valueOf(a).pow(b))
            }
        }
        println(s.size)
    }
}
