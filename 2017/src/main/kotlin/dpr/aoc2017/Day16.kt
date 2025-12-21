package dpr.aoc2017

import dpr.commons.Util

object Day16 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/16/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: String): String {
        val (a, acc) = parseInput(input)

        return acc.fold(a) { cur, action -> action.apply(cur) }.joinToString("")
    }

    private fun parseInput(input: String): Pair<List<Char>, List<Action>> {
        val range = 15

        val a = (0..range).map {
            'a' + it
        }

        val acc = input.split(",").map { action ->
            val type = action[0]
            val params = action.substring(1)
            when (type) {
                's' -> Action.Spin(params.toInt())
                'x' -> {
                    val parts = params.split('/')
                    Action.Exchange(p1 = parts[0].toInt(), p2 = parts[1].toInt())
                }

                'p' -> {
                    val parts = params.split('/')
                    Action.Partner(n1 = parts[0].first(), n2 = parts[1].first())
                }

                else -> throw RuntimeException(action)
            }
        }
        return Pair(a, acc)
    }

    @JvmStatic
    fun part2(input: String): String {
        var (a, acc) = parseInput(input)
        var iter = 0
        val mem = mutableMapOf<List<Char>, Int>()


//        return acc.fold(a) { cur, action -> action.apply(cur) }.joinToString("")

        val all = 1000000000
        while (iter < all) {
            ++iter
            a = acc.fold(a) { cur, action -> action.apply(cur) }
            if (a in mem) {
                val diff = iter - mem[a]!!
                val match = (all - iter) / diff
                iter += match * diff
            }
            mem[a] = iter
        }
        return a.joinToString("")
    }

    sealed class Action {
        abstract fun apply(a: List<Char>): List<Char>

        data class Partner(val n1: Char, val n2: Char) : Action() {
            override fun apply(a: List<Char>): List<Char> {
                val toEx = mutableMapOf<Char, Int>()
                (a.indices).forEach { i ->
                    if (a[i] == n1) {
                        toEx[n1] = i
                    }
                    if (a[i] == n2) {
                        toEx[n2] = i
                    }
                }
                return Exchange(p1 = toEx[n1]!!, p2 = toEx[n2]!!).apply(a)
            }

        }

        data class Spin(val s: Int) : Action() {
            override fun apply(a: List<Char>): List<Char> {
                return a.drop(a.size - s) + a.take(a.size - s)
            }
        }

        data class Exchange(val p1: Int, val p2: Int) : Action() {
            override fun apply(a: List<Char>): List<Char> {
                val newA = mutableListOf<Char>()
                (a.indices).forEach { i ->
                    when (i) {
                        p1 -> newA.add(a[p2])
                        p2 -> newA.add(a[p1])
                        else -> newA.add(a[i])
                    }
                }
                return newA
            }
        }
    }
}
