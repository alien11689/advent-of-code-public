package pl.touk.dpr.aoc2019

import kotlin.math.absoluteValue

object Day12 {
    @JvmStatic
    fun main(args: Array<String>) {
        println(part1())
        println(part2())
    }

    private fun part1(): Any {
        val xxs = mutableListOf(
            listOf(-1, 7, 3),
            listOf(12, 2, -13),
            listOf(14, 18, -8),
            listOf(17, 4, -4),
        )

        val vvs = mutableListOf(
            listOf(0, 0, 0),
            listOf(0, 0, 0),
            listOf(0, 0, 0),
            listOf(0, 0, 0),
        )

        (1..1000).forEach {
            for (i in 0 until xxs.size) {
                var curX = xxs[i]
                var curV = vvs[i]
                vvs[i] = listOf(curV[0] + xxs.sumBy { signum(curX[0], it[0]) },
                    curV[1] + xxs.sumBy { signum(curX[1], it[1]) },
                    curV[2] + xxs.sumBy { signum(curX[2], it[2]) })
            }
            for (i in 0 until xxs.size) {
                var curX = xxs[i]
                xxs[i] = listOf(curX[0] + vvs[i][0], curX[1] + vvs[i][1], curX[2] + vvs[i][2])
            }
        }

        var sum = 0
        for (i in 0 until xxs.size) {
            sum += xxs[i].sumBy { it.absoluteValue } * vvs[i].sumBy { it.absoluteValue }
        }

        return sum
    }

    private fun part2(): Any {
        val xxs = mutableListOf(
            listOf(-1, 7, 3),
            listOf(12, 2, -13),
            listOf(14, 18, -8),
            listOf(17, 4, -4),
        )

        val vvs = mutableListOf(
            listOf(0, 0, 0),
            listOf(0, 0, 0),
            listOf(0, 0, 0),
            listOf(0, 0, 0),
        )

        var iter = 0
        var nextX = 0
        var nextY = 0
        var nextZ = 0
        while (true) {
            ++iter
            for (i in 0 until xxs.size) {
                val curX = xxs[i]
                val curV = vvs[i]
                vvs[i] = listOf(curV[0] + xxs.sumBy { signum(curX[0], it[0]) },
                    curV[1] + xxs.sumBy { signum(curX[1], it[1]) },
                    curV[2] + xxs.sumBy { signum(curX[2], it[2]) })
            }
            for (i in 0 until xxs.size) {
                val curX = xxs[i]
                xxs[i] = listOf(curX[0] + vvs[i][0], curX[1] + vvs[i][1], curX[2] + vvs[i][2])
            }
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

    private fun signum(a: Int, b: Int): Int {
        if (a == b) {
            return 0
        }
        return if (a < b) 1 else -1
    }
}