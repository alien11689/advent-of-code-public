package dpr.eulerproject

import dpr.commons.Util

object Problem0002 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val res = generateSequence(0 to 1) { (it.second to (it.first + it.second)) }
            .takeWhile { it.second < 4_000_000 }
            .filter { it.second % 2 == 0 }
            .sumOf { it.second.toLong() }
        println(res)
    }
}
