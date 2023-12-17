package dpr.eulerproject

import dpr.commons.Util

object Problem0015 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val mem = mutableMapOf<Pair<Int, Int>, Long>()
        println(paths(20, 20, mem))
    }

    private fun paths(n: Int, m: Int, mem: MutableMap<Pair<Int, Int>, Long>): Long {
        val key = n to m
        if (key in mem) {
            return mem[key]!!
        }
        if (n == 0 || m == 0) {
            mem[key] = 1
            return 1
        }
        val paths = paths(n - 1, m, mem) + paths(n, m - 1, mem)
        mem[key] = paths
        return paths
    }
}
