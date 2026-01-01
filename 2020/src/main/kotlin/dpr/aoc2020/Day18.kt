package dpr.aoc2020

import dpr.commons.Util
import java.util.Stack

object Day18 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/18/input.txt")
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: List<String>): Long {
        return input.sumOf { solve(it) }
    }

    private fun solve(expression: String): Long {
        val parts = expression
            .flatMap { if (it in setOf('(', ')')) listOf(' ', it, ' ') else listOf(it) }
            .joinToString(separator = "")
            .trim()
            .split(Regex(" +"))
        return solveInternal(parts)
    }

    private fun solveInternal(parts: List<String>): Long {
        val stack = Stack<Pair<Long, Operation>>()
        var res = -1L
        var curOp = Operation.None
        parts.forEach { p ->
            when (p) {
                "(" -> {
                    stack.push(Pair(res, curOp))
                    res = -1L
                }

                ")" -> {
                    val (prevRes, operation) = stack.pop()
                    if (prevRes > -1) {
                        res = operation.calc(prevRes, res)
                        curOp = Operation.None
                    }
                }

                "+" -> {
                    curOp = Operation.Plus
                }

                "*" -> {
                    curOp = Operation.Times
                }

                else -> {
                    res = if (res > -1) {
                        curOp.calc(res, p.toLong())
                    } else {
                        p.toLong()
                    }
                }
            }
        }
        return res
    }

    @JvmStatic
    fun part2(input: List<String>): Long {
        return input.sumOf { solve2(it) }
    }

    enum class Operation {
        None,
        Plus,
        Times;

        fun calc(prevRes: Long, res: Long): Long =
            when (this) {
                None -> throw RuntimeException("Cannot apply none on $prevRes, $res")
                Plus -> prevRes + res
                Times -> prevRes * res
            }
    }

    private fun solve2(expression: String): Long {
        val parts = expression
            .flatMap {
                when (it) {
                    '(' -> listOf(' ', it, ' ', it, ' ')
                    ')' -> listOf(' ', it, ' ', it, ' ')
                    '*' -> listOf(' ', ')', ' ', '*', ' ', '(')
                    else -> listOf(it)
                }
            }
            .joinToString(separator = "", prefix = "( ", postfix = " )")
            .trim()
            .split(Regex(" +"))
        return solveInternal(parts)
    }

}
