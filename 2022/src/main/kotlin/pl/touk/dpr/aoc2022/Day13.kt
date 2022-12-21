package pl.touk.dpr.aoc2022

import java.util.StringTokenizer

object Day13 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/13/input.txt")
        println("Part 1:")
//        println(part1(Util.getNotEmptyLinesFromFile("/13/test1.txt")))
        println(part1(lines))
        println("Part 2:")
//        println(part2(Util.getNotEmptyLinesFromFile("/13/test1.txt")))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val pairs = parseExpressions(lines).chunked(2)
        return pairs.mapIndexed { i, pair ->
            if (pair.first() <= pair.last()) {
                i + 1
            } else 0
        }.sum()
    }

    sealed interface Expr : Comparable<Expr> {
        override fun compareTo(other: Expr): Int {
            if (this is V && other is V) {
                return i.compareTo(other.i)
            } else if (this is L && other is L) {
                l.indices.map { i ->
                    if (i >= other.l.size) {
                        return 1
                    }
                    when (val comparedItem = l[i].compareTo(other.l[i])) {
                        0 -> {}
                        else -> return comparedItem
                    }
                }
                return l.size.compareTo(other.l.size)
            } else {
                return lift().compareTo(other.lift())
            }
        }

        fun lift(): L

        data class V(val i: Int) : Expr {
            override fun lift() = L(listOf(this))
        }

        data class L(val l: List<Expr>) : Expr {
            override fun lift() = this
        }
    }

    private fun parseExpressions(lines: List<String>): List<Expr> =
        lines.map {
            val stringTokenizer = StringTokenizer(it, ",[]", true)
            parseExpression(stringTokenizer.nextToken(), stringTokenizer)
        }

    private fun parseExpression(token: String, tokens: StringTokenizer): Expr =
        when (token) {
            "[" -> readContainer(tokens)
            else -> Expr.V(token.toInt())
        }

    private fun readContainer(tokens: StringTokenizer): Expr {
        val parts = mutableListOf<Expr>()
        while (tokens.hasMoreTokens()) {
            when (val token = tokens.nextToken()) {
                "," -> continue
                "]" -> return Expr.L(parts)
                else -> parts.add(parseExpression(token, tokens))
            }
        }
        throw RuntimeException("Unfinished")
    }

    private fun part2(lines: List<String>): Any {
        val firstPivot = Expr.L(listOf(Expr.L(listOf(Expr.V(2)))))
        val secondPivot = Expr.L(listOf(Expr.L(listOf(Expr.V(6)))))
        val expressions = parseExpressions(lines) + listOf(firstPivot, secondPivot)
        val sorted = expressions.sorted()
        return (sorted.indexOf(firstPivot) + 1) * (sorted.indexOf(secondPivot) + 1)
    }
}

