package dpr.aoc2019

import dpr.commons.Util
import kotlin.math.absoluteValue

object Day12 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        println(part1())
        println(part2())
    }

    @JvmStatic
    fun part1(): Int {
        val xxs = initXXS()

        val vvs = initVVS()

        repeat(1000) {
            iteration(xxs, vvs)
        }

        var sum = 0
        for (i in 0 until xxs.size) {
            sum += xxs[i].sumOf { it.absoluteValue } * vvs[i].sumOf { it.absoluteValue }
        }

        return sum
    }

    @JvmStatic
    fun part2(): Long {
        val xxs = initXXS()

        val vvs = initVVS()

        var iter = 0
        var nextX = 0
        var nextY = 0
        var nextZ = 0
        while (true) {
            ++iter
            iteration(xxs, vvs)
            if (nextX == 0 && vvs.all { it[0] == 0 }) {
//                println "$iter -> x"
                nextX = iter
            }
            if (nextY == 0 && vvs.all { it[1] == 0 }) {
//                println "$iter -> y"
                nextY = iter
            }
            if (nextZ == 0 && vvs.all { it[2] == 0 }) {
//                println "$iter -> z"
                nextZ = iter
            }
            if (nextX > 0 && nextY > 0 && nextZ > 0) {
//                println("$nextX, $nextY, $nextZ")
//                println("il: " + (nextX * nextY * nextZ))
                return 1L * nextX * nextY * nextZ
            }
        }
    }

    private fun iteration(xxs: MutableList<List<Int>>, vvs: MutableList<List<Int>>) {
        for (i in 0 until xxs.size) {
            val curX = xxs[i]
            val curV = vvs[i]
            vvs[i] = listOf(curV[0] + xxs.sumOf { signum(curX[0], it[0]) },
                curV[1] + xxs.sumOf { signum(curX[1], it[1]) },
                curV[2] + xxs.sumOf { signum(curX[2], it[2]) })
        }
        for (i in 0 until xxs.size) {
            val curX = xxs[i]
            xxs[i] = listOf(curX[0] + vvs[i][0], curX[1] + vvs[i][1], curX[2] + vvs[i][2])
        }
    }

    private fun initVVS() = mutableListOf(
        listOf(0, 0, 0),
        listOf(0, 0, 0),
        listOf(0, 0, 0),
        listOf(0, 0, 0),
    )

    private fun initXXS() = mutableListOf(
        listOf(-1, 7, 3),
        listOf(12, 2, -13),
        listOf(14, 18, -8),
        listOf(17, 4, -4),
    )

    private fun signum(a: Int, b: Int): Int {
        if (a == b) {
            return 0
        }
        return if (a < b) 1 else -1
    }
}
