package pl.touk.dpr.eulerproject

import java.time.DayOfWeek
import java.time.LocalDate

object Problem0019 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val res = (1901..2000).sumOf { year ->
            (1..12).count { month -> LocalDate.of(year, month, 1).dayOfWeek == DayOfWeek.SUNDAY }
        }
        println(res)
    }
}
