package dpr.eulerproject

import dpr.commons.Dir
import dpr.commons.Util

object Problem0028 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        var maxRight = 0
        var maxLeft = 0
        var maxUp = 0
        var maxDown = 0
        var cur = 0 to 0
        var sumDiagonal = 0L
        var value = 1L
        var dir = Dir.E

        val limit = 1001 * 1001
//        val limit = 5*5

        while (value <= limit) {
            when (dir) {
                Dir.E -> {
                    val x = cur.first
                    cur = (++maxRight) to cur.second
                    value += cur.first - x
                    dir = Dir.S
                    sumDiagonal += value - 1
                }

                Dir.W -> {
                    val x = cur.first
                    cur = (--maxLeft) to cur.second
                    value += x - cur.first
                    dir = Dir.N
                    sumDiagonal += value
                }

                Dir.N -> {
                    val y = cur.second
                    cur = cur.first to (--maxUp)
                    value += y - cur.second
                    dir = Dir.E
                    sumDiagonal += value
                }

                Dir.S -> {
                    val y = cur.second
                    cur = cur.first to (++maxDown)
                    value += cur.second - y
                    dir = Dir.W
                    sumDiagonal += value
                }
            }
        }
        println(sumDiagonal)

    }
}
