package pl.touk.dpr.aoc2020.day10

import pl.touk.dpr.aoc2020.Util
import java.util.Stack

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
//        val input = Util.getFileContent("/10/test.txt")
//        val input = Util.getFileContent("/10/test2.txt")
        val input = Util.getNotEmptyLinesFromFile("/10/input.txt")
                .map { it.toInt() }
        part1(input)
        part2(input)
    }

    private fun part1(input: List<Int>) {
        val sorted = input.sorted() + (input.maxOrNull()!! + 3)
        var current = 0
        var difference1 = 0
        var difference3 = 0
        sorted.forEach { num ->
            val diff = num - current
            when (diff) {
                1 -> ++difference1
                3 -> ++difference3
                else -> {
                }
            }
            current = num
        }
        println(difference1 * difference3)
    }

    private fun part2(input: List<Int>) {
        val sorted = input.sorted() + (input.maxOrNull()!! + 3)
        var base = 0
        var current = 0
        var set = mutableListOf<Int>()
        val groups = mutableSetOf<Group>()
        sorted.forEach { num ->
            val diff = num - current
            when (diff) {
                3 -> {
                    groups.add(Group(base, current, set - current))
                    set = mutableListOf()
                    base = num
                }
                else -> {
                    set.add(num)
                }
            }
            current = num
        }

        val res = groups.map { it.calculate() }.fold(1L, { acc, l -> acc * l })
        println(res)
    }

    data class Group(val from: Int, val to: Int, val adapters: List<Int>) {
        fun calculate(): Int {
            val stack = Stack<Entry>()
            val paths = mutableSetOf<List<Int>>()
            stack.push(Entry(from, adapters, listOf()))
            while (stack.isNotEmpty()) {
                val (current, next, path) = stack.pop()
                if (current + 3 >= to) {
                    paths.add(path)
                }
                if (!next.isEmpty()) {
                    val first = next.first()
                    val tail = next.subList(1, next.size)
                    if (current + 3 >= first) {
                        stack.push(Entry(first, tail, path + first))
                        stack.push(Entry(current, tail, path))
                    }
                }
            }
            return paths.size
        }
    }

    data class Entry(val current: Int, val next: List<Int>, val path: List<Int>)
}