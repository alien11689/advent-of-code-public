package pl.touk.dpr.eulerproject

import java.math.BigInteger
import java.time.DayOfWeek
import java.time.LocalDate
import kotlin.math.sqrt

object Problem0021 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val mem = mutableMapOf<Int, Int>()
        var sum = 0
        generateSequence(2) {it + 1}
                .takeWhile { it < 10000 }
                .forEach { cur ->
                    val properDivisors = properDividers(cur).sum()
                    if (properDivisors > 0){
                        if(mem[properDivisors] == cur){
                            sum += cur + properDivisors
                        }
                        mem[cur] = properDivisors
                    }
                }
        println(sum)
    }

    private fun properDividers(value: Int): Set<Int> {
        val divs = mutableSetOf(1)
        var i = 2
        while (i <= sqrt(value.toDouble())) {
            if (value % i == 0) {
                divs.add(i)
                divs.add(value / i)
            }
            ++i
        }
        return divs
    }
}
