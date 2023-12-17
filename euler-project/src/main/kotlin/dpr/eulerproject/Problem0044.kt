package dpr.eulerproject

import dpr.commons.Util

object Problem0044 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {

        val prevPentagonal = mutableMapOf(1 to pentagonal(1))
        val possiblePentagonal = mutableMapOf<Long, Set<Pair<Int, Int>>>()

        var i = 2
        while (i < 10000) {
//            println("Checking $i with limit $limit")
            val p = pentagonal(i)
            if (p in possiblePentagonal) {
                val res = possiblePentagonal[p]!!
//                println("$res can be possible solution - diff is ${res.minOf { it.second - it.first }}")
                println(res.minOf { pentagonal(it.second) - pentagonal(it.first) })
                break
            }
            prevPentagonal.forEach {
                if ((p - it.value) in prevPentagonal.values) {
                    val expectedValue = p + it.value
//                    println("P${it.key} = ${it.value} and P$i = $p - (score is ${i - it.key}) and waiting for pentagonal equal to $expectedValue")
                    possiblePentagonal[expectedValue] = (possiblePentagonal[expectedValue]
                        ?: emptySet()) + (it.key to i)
                }
            }
            prevPentagonal[i] = p
            ++i
        }
    }

    private fun pentagonal(i: Int): Long = i * (3L * i - 1) / 2

}
