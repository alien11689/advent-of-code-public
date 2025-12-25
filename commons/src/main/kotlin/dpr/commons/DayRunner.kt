package dpr.commons

import java.util.ServiceLoader
import java.util.function.Function

object DayRunner {
    @JvmStatic
    fun findAndRunDays(args: Array<String?>) {
        var days = ServiceLoader.load(Day::class.java).stream()
            .map { it.get() }
        if (args.size > 0) {
            days = days.filter { d: Day -> d.dayNum() == args[0]!!.toInt() }
        }
        val finalDays = days
        Util.measureTime(Runnable {
            finalDays
                .sorted(Comparator.comparing(Function { obj: Day -> obj.dayNum() }))
                .forEach { d: Day ->
                    System.out.printf("Day%02d%n", d.dayNum())
                    Util.measureTime { d.execute() }
                }
        })
    }
}