package pl.touk.dpr.eulerproject

import java.lang.StringBuilder
import java.math.BigInteger

object Problem0026 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val res = generateSequence(2){ it + 1 }
                .takeWhile { it < 1000 }
                .maxBy { repeatingDecimalPoints(it) }

        println(res)
    }

    private fun repeatingDecimalPoints(number: Int):Int {
//        println("Checking $number")
        var x = 1
        val sb = StringBuilder()
        val mem = mutableMapOf<Pair<Int,Int>, Int>()
        while(true) {
            while (x < number) {
                x *= 10
            }
            if (x % number == 0) {
//                println("Result 0")
                return 0
            }
            val div = x / number
            val rem = x % number
            if ((div to rem) in mem) {
                val res = sb.length - mem[div to rem]!!
//                println("Result $res")
                return res
            }
            mem[div to rem] = sb.length
            sb.append(div)
            x = rem
        }
    }
}
