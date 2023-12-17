package dpr.eulerproject

import dpr.commons.Util
import kotlin.math.absoluteValue

object Problem0006 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val sum100 = (1..100L).sum()
        val s1 = sum100 * sum100
        val s2 = (1..100L).sumOf { it * it }
        println((s2 - s1).absoluteValue)
    }
}
