package pl.touk.dpr.aoc2021

import java.util.Stack

object Day10 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/10/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        return lines.map { findIllegalCharacter(it).second }.mapNotNull {
            when (it) {
                ')' -> 3L
                '}' -> 1197L
                ']' -> 57L
                '>' -> 25137L
                else -> 0L
            }
        }.sum()
    }

    private fun findIllegalCharacter(line: String): Pair<Stack<Char>, Char?> {
        val stack = Stack<Char>()
        line.forEach {
            when (it) {
                in setOf('(', '[', '{', '<') -> stack.push(it)
                ')' -> {
                    val last = stack.peek()
                    if (last != '(') return Pair(stack, it)
                    stack.pop()
                }
                '}' -> {
                    val last = stack.peek()
                    if (last != '{') return Pair(stack, it)
                    stack.pop()
                }
                '>' -> {
                    val last = stack.peek()
                    if (last != '<') return Pair(stack, it)
                    stack.pop()
                }
                ']' -> {
                    val last = stack.peek()
                    if (last != '[') return Pair(stack, it)
                    stack.pop()
                }
            }
        }
        return Pair(stack, null)
    }

    private fun part2(lines: List<String>): Any {
        val results = lines.map { findIllegalCharacter(it) }
            .filter { it.second == null }
            .map { score(it.first) }
            .sorted()
//        println(results)
        return results[results.size / 2]
    }

    private fun score(stack: Stack<Char>): Long {
        var sum = 0L
        while (stack.isNotEmpty()) {
            val cur = stack.pop()
            sum *= 5
            sum += when (cur) {
                '(' -> 1L
                '[' -> 2L
                '{' -> 3L
                '<' -> 4L
                else -> throw RuntimeException()
            }
        }
        return sum
    }
}

