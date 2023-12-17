package dpr.eulerproject

import dpr.commons.Util

object Problem0014 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val mem = mutableMapOf(1L to 1)
        var i = 2L
        while (i < 1_000_000) {
            var j = i
            var iter = 1
            while (j != 1L) {
                if (j in mem) {
                    iter += mem[j]!!
                    break
                }
                j = when {
                    j % 2 == 0L -> j / 2
                    else -> 3 * j + 1
                }
                ++iter
            }
            mem[i] = iter
            ++i
        }
        println(mem.maxBy { it.value }.key)
    }
}
