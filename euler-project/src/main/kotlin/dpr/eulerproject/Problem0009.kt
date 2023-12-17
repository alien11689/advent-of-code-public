package dpr.eulerproject

import dpr.commons.Util

object Problem0009 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        for (a in 1..1000) {
            for (b in a..1000) {
                val c = 1000 - a - b
                if (c <= b) {
                    break
                }
                if (c * c == a * a + b * b) {
                    println(a * b * c)
                    return@measureTime
                }
            }
        }
    }
}
