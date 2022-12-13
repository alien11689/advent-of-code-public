package pl.touk.dpr.aoc2022

import java.util.StringTokenizer

object Day13 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/13/input.txt")
        println("Part 1:")
        println(part1(Util.getNotEmptyLinesFromFile("/13/test1.txt")))
        println(part1(lines))
        println("Part 2:")
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val pairs = parseExpressions(lines).chunked(2)

        return pairs.mapIndexed { i, pair ->
//            println("Comparing pair $pair")
            if (pair.first().compareTo(pair.last()) < 1) {
                i + 1
            } else 0
        }.sum()
    }

    sealed interface Expr : Comparable<Expr> {
        override fun compareTo(second: Expr): Int {
            if (this is V && second is V) {
                return i.compareTo(second.i)
            } else if (this is L && second is L) {
                if (l.isEmpty() && second.l.isNotEmpty()) {
                    return -1
                } else if (l.isNotEmpty() && second.l.isEmpty()) {
                    return 1
                }
                this.l.indices.map { i ->
                    if (i >= second.l.size) {
                        return 1
                    }
                    val comparedItem = l[i].compareTo(second.l[i])
                    when (comparedItem) {
                        -1, 1 -> return comparedItem
                        else -> {}
                    }
                }
                if (l.size == second.l.size) {
                    return 0
                } else if (l.size < second.l.size) {
                    return -1
                } else {
                    throw RuntimeException("Here")
                }
            } else {
                return this.lift().compareTo(second.lift())
            }
        }

        fun lift(): Expr

        data class V(val i: Int) : Expr {
            override fun lift(): Expr = L(listOf(this))
        }

        data class L(val l: List<Expr>) : Expr {
            override fun lift(): Expr = this
        }
    }

    private fun parseExpressions(lines: List<String>): List<Expr> {
        return lines.map {
            val stringTokenizer = StringTokenizer(it, ",[]", true)
            parseExpression(stringTokenizer.nextToken(), stringTokenizer)
        }
    }

    private fun parseExpression(token: String, tokens: StringTokenizer): Expr {
        val expr = when (token) {
            "[" -> readContainer(tokens)
            else -> Expr.V(token.toInt())
        }
        return expr
    }

    private fun readContainer(tokens: StringTokenizer): Expr {
        val parts = mutableListOf<Expr>()
        while (tokens.hasMoreTokens()) {
            val token = tokens.nextToken()
            when (token) {
                "," -> continue
                "]" -> return Expr.L(parts.toList())
                else -> parts.add(parseExpression(token, tokens))
            }
        }
        throw RuntimeException("Unfinished")
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

