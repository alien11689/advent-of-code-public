package pl.touk.dpr.aoc2020.day01

import pl.touk.dpr.aoc2020.Util

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getFileContent("/01/input.txt")
        part1(input)
        part2(input)
    }

    private fun part2(input: String) {
        val nums = input.lines().filter { it.isNotEmpty() }.map { it.toInt() }
        nums.withIndex().forEach {
            val a = it.value
            val i = it.index
            nums.withIndex().filter { it.index > i }.forEach {
                val b = it.value
                val i = it.index
                nums.withIndex().filter { it.index > i }.forEach {
                    val c = it.value
                    if (a + b + c == 2020) {
                        println(a * b * c)
                        return
                    }
                }
            }
        }
    }

    private fun part1(input: String) {
        val mem = mutableSetOf<Int>()
        println(input.lines()
                .filter { it.isNotEmpty() }
                .flatMap { s ->
                    val n = s.toInt()
                    val compliant = 2020 - n;
                    if (compliant in mem) {
                        listOf(n * compliant)
                    } else {
                        mem.add(n)
                        listOf()
                    }
                }
                .take(1))
    }

}

