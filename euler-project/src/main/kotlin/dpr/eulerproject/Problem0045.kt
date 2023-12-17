package dpr.eulerproject

import dpr.commons.Util

object Problem0045 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        var n = 1
        val triangles = mutableMapOf<Int, Long>()
        val pentagonals = mutableMapOf<Int, Long>()
        val hexagonals = mutableMapOf<Int, Long>()
        var limit = 285
        while (true) {
            while (n <= limit) {
                triangles[n] = triangle(n)
                pentagonals[n] = pentagonal(n)
                hexagonals[n] = hexagonal(n)
                ++n
            }
            val intersections = triangles.values.intersect(pentagonals.values.toSet()).intersect(hexagonals.values.toSet())
            if (intersections.size > 2) {
                println(intersections.max())
                break
            }
            limit += 10000
//            println("Increasing limit to $limit")
        }
    }


    fun triangle(n: Int): Long = n.toLong() * (n + 1) / 2
    fun pentagonal(n: Int): Long = n.toLong() * (3 * n - 1) / 2
    fun hexagonal(n: Int): Long = n.toLong() * (2 * n - 1)
}
