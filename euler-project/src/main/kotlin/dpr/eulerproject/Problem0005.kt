package dpr.eulerproject

import dpr.commons.Util

object Problem0005 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val step = 2L * 3 * 5 * 7 * 11 * 13 * 17 * 19
        val res = generateSequence(step) { it + step }
            .first { cur -> (4..20).all { cur % it == 0L } }
        println(res)
    }
}
