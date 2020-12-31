package pl.touk.dpr.aoc2019

object Day04 {
    @JvmStatic
    fun main(args: Array<String>) {
        println(part1())
        println(part2())
    }

    private fun part1(): Any {
        var start = 156218
        val end = 652527
        var c = 0
        while (start <= end) {
            if (isPassword1(start.toString().map { Integer.parseInt(it.toString()) })) {
                ++c
            }
            ++start
        }
        return c
    }

    private fun part2(): Any {
        var start = 156218
        val end = 652527
        var c = 0
        while (start <= end) {
            if (isPassword2(start.toString().map { Integer.parseInt(it.toString()) })) {
                ++c
            }
            ++start
        }
        return c
    }

    fun isPassword1(password: List<Int>): Boolean {
        val m = mutableMapOf(Pair(password[0], 1))
        for (i in 1 until password.size) {
            if (password[i - 1] > password[i]) {
                return false
            }
            if (password[i] in m) {
                m[password[i]] = m[password[i]]!! + 1
            } else {
                m[password[i]] = 1
            }
        }

        return m.values.find { it >= 2 } != null
    }

    fun isPassword2(password: List<Int>): Boolean {
        val m = mutableMapOf(Pair(password[0], 1))
        for (i in 1 until password.size) {
            if (password[i - 1] > password[i]) {
                return false
            }
            if (password[i] in m) {
                m[password[i]] = m[password[i]]!! + 1
            } else {
                m[password[i]] = 1
            }
        }

        return m.values.find { it == 2 } != null
    }

}