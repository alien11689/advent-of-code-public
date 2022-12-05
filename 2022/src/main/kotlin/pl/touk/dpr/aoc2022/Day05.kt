package pl.touk.dpr.aoc2022

import java.util.Stack

object Day05 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/05/input.txt")
        println("Part 1:")
        println(part1(lines))
        println("Part 2:")
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
//        [T]             [P]     [J]
//        [F]     [S]     [T]     [R]     [B]
//        [V]     [M] [H] [S]     [F]     [R]
//        [Z]     [P] [Q] [B]     [S] [W] [P]
//        [C]     [Q] [R] [D] [Z] [N] [H] [Q]
//        [W] [B] [T] [F] [L] [T] [M] [F] [T]
//        [S] [R] [Z] [V] [G] [R] [Q] [N] [Z]
//        [Q] [Q] [B] [D] [J] [W] [H] [R] [J]
//        1   2   3   4   5   6   7   8   9
        val stacks = listOf(
            Stack<Char>().also {
                it.push('Q')
                it.push('S')
                it.push('W')
                it.push('C')
                it.push('Z')
                it.push('V')
                it.push('F')
                it.push('T')
            },
            Stack<Char>().also {
                it.push('Q')
                it.push('R')
                it.push('B')
            },
            Stack<Char>().also {
                it.push('B')
                it.push('Z')
                it.push('T')
                it.push('Q')
                it.push('P')
                it.push('M')
                it.push('S')
            },
            Stack<Char>().also {
                it.push('D')
                it.push('V')
                it.push('F')
                it.push('R')
                it.push('Q')
                it.push('H')
            },
            Stack<Char>().also {
                it.push('J')
                it.push('G')
                it.push('L')
                it.push('D')
                it.push('B')
                it.push('S')
                it.push('T')
                it.push('P')
            },
            Stack<Char>().also {
                it.push('W')
                it.push('R')
                it.push('T')
                it.push('Z')
            },
            Stack<Char>().also { st -> "HQMNSFRJ".forEach { st.push(it) } },
            Stack<Char>().also { st -> "RNFHW".forEach { st.push(it) } },
            Stack<Char>().also { st -> "JZTQPRB".forEach { st.push(it) } },
        )
        lines.filter { it.startsWith("move") }
            .forEach {
                val parts = it.split(" ")
                val count = parts[1].toInt()
                val from = parts[3].toInt()
                val to = parts[5].toInt()
                (1..count).forEach {
                    stacks[to - 1].push(stacks[from - 1].pop())
                }
            }
        return stacks.map { it.peek() }.joinToString("")
    }

    private fun part2(lines: List<String>): Any {
        val stacks = listOf(
            Stack<Char>().also {
                it.push('Q')
                it.push('S')
                it.push('W')
                it.push('C')
                it.push('Z')
                it.push('V')
                it.push('F')
                it.push('T')
            },
            Stack<Char>().also {
                it.push('Q')
                it.push('R')
                it.push('B')
            },
            Stack<Char>().also {
                it.push('B')
                it.push('Z')
                it.push('T')
                it.push('Q')
                it.push('P')
                it.push('M')
                it.push('S')
            },
            Stack<Char>().also {
                it.push('D')
                it.push('V')
                it.push('F')
                it.push('R')
                it.push('Q')
                it.push('H')
            },
            Stack<Char>().also {
                it.push('J')
                it.push('G')
                it.push('L')
                it.push('D')
                it.push('B')
                it.push('S')
                it.push('T')
                it.push('P')
            },
            Stack<Char>().also {
                it.push('W')
                it.push('R')
                it.push('T')
                it.push('Z')
            },
            Stack<Char>().also { st -> "HQMNSFRJ".forEach { st.push(it) } },
            Stack<Char>().also { st -> "RNFHW".forEach { st.push(it) } },
            Stack<Char>().also { st -> "JZTQPRB".forEach { st.push(it) } },
        )
        lines.filter { it.startsWith("move") }
            .forEach {
                val parts = it.split(" ")
                val count = parts[1].toInt()
                val from = parts[3].toInt()
                val to = parts[5].toInt()
                val local = Stack<Char>()
                (1..count).forEach {
                    local.push(stacks[from - 1].pop())
                }
                while (local.isNotEmpty()) {
                    stacks[to - 1].push(local.pop())
                }
            }
        return stacks.map { it.peek() }.joinToString("")
    }
}

