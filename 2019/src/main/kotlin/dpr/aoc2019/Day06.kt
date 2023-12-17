package dpr.aoc2019

import dpr.commons.Util
import java.util.Stack

object Day06 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/06/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val v = input.map { it.split(')') }.toMutableList()
        val m = mutableMapOf(Pair("COM", 0))

        val stack = Stack<String>()
        stack.push("COM")

        while (stack.isNotEmpty()) {
            val cur = stack.pop()
            val count = m[cur]!!
            val nodes = v.filter { it[0] == cur }
            v.removeAll(nodes)
            nodes.forEach { n ->
                stack.push(n[1])
                m[n[1]] = count + 1
            }
        }
        return m.values.filter { it > 0 }.sum()
    }

    private fun part2(input: List<String>): Any {
        val v = input.map { it.split(')') }.toMutableList()
        val m = mutableMapOf(Pair("YOU", 0))

        val stack = Stack<String>()
        stack.push("YOU")

        while (stack.isNotEmpty()) {
            val cur = stack.pop()
            val count = m[cur]!!
            var nodes = v.filter { it[0] == cur }
            v.removeAll(nodes)
            nodes.forEach { n ->
                if (n[1] !in m) {
                    stack.push(n[1])
                    m[n[1]] = count + 1
                }
            }
            nodes = v.filter { it[1] == cur }
            v.removeAll(nodes)
            nodes.forEach { n ->
                if (n[0] !in m) {
                    stack.push(n[0])
                    m[n[0]] = count + 1
                }
            }
        }
        return m["SAN"]!! - 2
    }
}
