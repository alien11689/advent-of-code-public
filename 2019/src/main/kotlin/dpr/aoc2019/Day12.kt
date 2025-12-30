package dpr.aoc2019

import dpr.commons.Util
import java.lang.Integer.parseInt
import kotlin.math.abs
import kotlin.math.absoluteValue

object Day12 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/12/input.txt")
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: List<String>, steps: Int = 1000): Int {
        val xxs = parseInput(input)

        val vvs = initVVS()

        repeat(steps) {
            iteration(xxs, vvs)
        }

        var sum = 0
        for (i in 0 until xxs.size) {
            sum += xxs[i].sumOf { it.absoluteValue } * vvs[i].sumOf { it.absoluteValue }
        }

        return sum
    }

    private fun parseInput(input: List<String>): MutableList<List<Int>> = input.map {
        val parts = it.split(Regex("[=,>]+"))
        listOf(parseInt(parts[1]), parseInt(parts[3]), parseInt(parts[5]))
    }.toMutableList()

    @JvmStatic
    fun part2(input: List<String>): Long {
        val xxs = parseInput(input)
        val initXXS = xxs.map { it }

        val vvs = initVVS()

        var iter = 0
        var nextX = 0
        var nextY = 0
        var nextZ = 0
        while (true) {
            ++iter
            iteration(xxs, vvs)
            if (nextX == 0 && vvs.all { it[0] == 0 } && xxs.map { it[0] } == initXXS.map { it[0] }) {
//                println("$iter -> x")
                nextX = iter
            }
            if (nextY == 0 && vvs.all { it[1] == 0 } && xxs.map { it[1] } == initXXS.map { it[1] }) {
//                println("$iter -> y")
                nextY = iter
            }
            if (nextZ == 0 && vvs.all { it[2] == 0 } && xxs.map { it[2] } == initXXS.map { it[2] }) {
//                println("$iter -> z")
                nextZ = iter
            }
            if (nextX > 0 && nextY > 0 && nextZ > 0) {
//                println("$nextX, $nextY, $nextZ")
//                println("il: " + (1L * nextX * nextY * nextZ))
//                println(
//                    "il2: " + (leastCommonMultiple(
//                        nextX.toLong(),
//                        leastCommonMultiple(nextY.toLong(), nextZ.toLong())
//                    ))
//                )
                return leastCommonMultiple(
                    nextX.toLong(), leastCommonMultiple(nextY.toLong(), nextZ.toLong())
                )
            }
        }
    }

    private fun iteration(xxs: MutableList<List<Int>>, vvs: MutableList<List<Int>>) {
        for (i in 0 until xxs.size) {
            val curX = xxs[i]
            val curV = vvs[i]
            vvs[i] = listOf(
                curV[0] + xxs.sumOf { signum(curX[0], it[0]) },
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

    private fun signum(a: Int, b: Int): Int {
        if (a == b) {
            return 0
        }
        return if (a < b) 1 else -1
    }

    private fun leastCommonMultiple(a: Long, b: Long): Long {
        if (a == 0L || b == 0L) {
            return 0
        }
        val absA = abs(a)
        val absB = abs(b)
        return (absA * absB) / greatestCommonDivisor(absA, absB)
    }

    private fun greatestCommonDivisor(a: Long, b: Long): Long {
        var a = a
        var b = b
        while (b != 0L) {
            val temp = b
            b = a % b
            a = temp
        }
        return a
    }
}
