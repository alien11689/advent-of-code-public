package dpr.aoc2022

import dpr.commons.Util
import java.util.Stack

object Day05 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/05/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    @JvmStatic fun part1(lines: List<String>): String {
        val stacks = initStacks()
        lines.filter { it.startsWith("move") }
            .forEach {
                val parts = it.split(" ")
                val count = parts[1].toInt()
                val from = parts[3].toInt()
                val to = parts[5].toInt()
                repeat(count) {
                    stacks[to - 1].push(stacks[from - 1].pop())
                }
            }
        return stacks.map { it.peek() }.joinToString("")
    }

    @JvmStatic fun part2(lines: List<String>): String {
        val stacks = initStacks()
        lines.filter { it.startsWith("move") }
            .forEach {
                val parts = it.split(" ")
                val count = parts[1].toInt()
                val from = parts[3].toInt()
                val to = parts[5].toInt()
                val local = Stack<Char>()
                repeat(count) {
                    local.push(stacks[from - 1].pop())
                }
                while (local.isNotEmpty()) {
                    stacks[to - 1].push(local.pop())
                }
            }
        return stacks.map { it.peek() }.joinToString("")
    }

    private fun initStacks(): List<Stack<Char>> = listOf(
        Stack<Char>().also { st -> "QSWCZVFT".forEach { st.push(it) } },
        Stack<Char>().also { st -> "QRB".forEach { st.push(it) } },
        Stack<Char>().also { st -> "BZTQPMS".forEach { st.push(it) } },
        Stack<Char>().also { st -> "DVFRQH".forEach { st.push(it) } },
        Stack<Char>().also { st -> "JGLDBSTP".forEach { st.push(it) } },
        Stack<Char>().also { st -> "WRTZ".forEach { st.push(it) } },
        Stack<Char>().also { st -> "HQMNSFRJ".forEach { st.push(it) } },
        Stack<Char>().also { st -> "RNFHW".forEach { st.push(it) } },
        Stack<Char>().also { st -> "JZTQPRB".forEach { st.push(it) } },
    )
}

