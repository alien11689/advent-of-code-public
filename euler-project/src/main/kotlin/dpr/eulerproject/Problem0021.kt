package dpr.eulerproject

import dpr.commons.MathUtil
import dpr.commons.Util

object Problem0021 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val mem = mutableMapOf<Int, Int>()
        var sum = 0
        generateSequence(2) { it + 1 }
            .takeWhile { it < 10000 }
            .forEach { cur ->
                val properDivisors = MathUtil.properDividers(cur).sum()
                if (properDivisors > 0) {
                    if (mem[properDivisors] == cur) {
                        sum += cur + properDivisors
                    }
                    mem[cur] = properDivisors
                }
            }
        println(sum)
    }
}
