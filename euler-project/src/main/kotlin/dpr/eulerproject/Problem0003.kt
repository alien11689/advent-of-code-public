package dpr.eulerproject

import dpr.commons.Util
import kotlin.math.sqrt

object Problem0003 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        var n = 600851475143L
        var i = 2
        while (i < sqrt(n.toDouble())) {
            if (n % i == 0L) {
                n /= i
            }
            ++i
        }
        println(n)
    }
}
