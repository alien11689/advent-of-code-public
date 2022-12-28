package pl.touk.dpr.eulerproject

import kotlin.math.sqrt

object Problem0012 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        var i = 1L
        var curSum = 1L
        while (true) {
            ++i
            curSum += i
            if (divisors(curSum).size > 500) {
                println(curSum)
                break
            }
        }
    }

    private fun divisors(value: Long): Set<Long> {
        val divs = mutableSetOf(1, value)
        var i = 2L
        while (i <= sqrt(value.toDouble())) {
            if (value % i == 0L) {
                divs.add(i)
                divs.add(value / i)
            }
            ++i
        }
        return divs
    }
}
