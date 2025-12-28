package dpr.aoc2019

import dpr.commons.Util

object Day04 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        println(part1())
        println(part2())
    }

    @JvmStatic
    fun part1(from: Int = 156218, end: Int = 652527): Int {
        var start = from
        var c = 0
        while (start <= end) {
            if (isPassword1(start.toString().map { Integer.parseInt(it.toString()) })) {
                ++c
            }
            ++start
        }
        return c
    }

    @JvmStatic
    fun part2(from: Int = 156218, end: Int = 652527): Int {
        var start = from
        var c = 0
        while (start <= end) {
            if (isPassword2(start.toString().map { Integer.parseInt(it.toString()) })) {
                ++c
            }
            ++start
        }
        return c
    }

    private fun isPassword1(password: List<Int>): Boolean {
        val m = generate(password)
        return m != null && m.values.find { it >= 2 } != null
    }

    private fun isPassword2(password: List<Int>): Boolean {
        val m = generate(password)
        return m != null && m.values.find { it == 2 } != null
    }

    private fun generate(password: List<Int>): Map<Int, Int>? {
        val m = mutableMapOf(Pair(password[0], 1))
        for (i in 1 until password.size) {
            if (password[i - 1] > password[i]) {
                return null
            }
            if (password[i] in m) {
                m[password[i]] = m[password[i]]!! + 1
            } else {
                m[password[i]] = 1
            }
        }
        return m
    }

}
