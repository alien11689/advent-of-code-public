package pl.touk.dpr.aoc2020.day18

import pl.touk.dpr.aoc2020.Util
import java.util.Stack

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/18/input.txt")
        println(part1(input))
//        Util.test(solve2("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"), 23340L)
//        Util.test(solve2("1 + (2 * 3) + (4 * (5 + 6))"), 51L)
//        Util.test(solve2("2 * 3 + (4 * 5)"), 46L)
//        Util.test(solve2("5 + (8 * 3 + 9 + 3 * 4 * 3)"), 1445L)
//        Util.test(solve2("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"), 669060L)
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        return input.map { solve(it) }.sum()
    }

    private fun solve(expression: String): Long {
        val parts = expression
                .flatMap { if (it in setOf('(', ')')) listOf(' ', it, ' ') else listOf(it) }
                .joinToString(separator = "")
                .trim()
                .split(Regex("[ ]+"))
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
                    if (res > -1) {
                        res = curOp.calc(res, p.toLong())
                    } else {
                        res = p.toLong()
                    }
                }
            }
        }
        return res
    }

    private fun part2(input: List<String>): Any {
        return input.map { solve2(it) }.sum()
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
                .split(Regex("[ ]+"))
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
                    if (res > -1) {
                        res = curOp.calc(res, p.toLong())
                    } else {
                        res = p.toLong()
                    }
                }
            }
        }
        return res
    }

}
