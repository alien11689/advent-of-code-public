package pl.touk.dpr.eulerproject

import java.util.stream.IntStream

object Problem0001 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val res = generateSequence(3) {it + 1}
                .takeWhile { it < 1000 }
                .filter { it % 3 == 0 || it % 5 == 0 }
                .sum()
        println(res)
    }
}
