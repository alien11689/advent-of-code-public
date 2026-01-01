package dpr.aoc2020

import dpr.commons.Util
import java.util.Stack
import kotlin.text.toInt

object Day10 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/10/input.txt")
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: List<String>): Int {
        val numbers = input.map { it.toInt() }
        val sorted = numbers.sorted() + (numbers.maxOrNull()!! + 3)
        var current = 0
        var difference1 = 0
        var difference3 = 0
        sorted.forEach { num ->
            when (num - current) {
                1 -> ++difference1
                3 -> ++difference3
                else -> {
                }
            }
            current = num
        }
        return difference1 * difference3
    }

    @JvmStatic
    fun part2(input: List<String>): Long {
        val numbers = input.map { it.toInt() }
        val sorted = numbers.sorted() + (numbers.maxOrNull()!! + 3)
        var base = 0
        var current = 0
        var set = mutableListOf<Int>()
        val groups = mutableSetOf<Group>()
        sorted.forEach { num ->
            when (num - current) {
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

        return groups.map { it.calculate() }.fold(1L) { acc, l -> acc * l }
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
                if (next.isNotEmpty()) {
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
