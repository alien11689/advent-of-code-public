package dpr.aoc2018

import java.util.Stack

object Day20 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/20/input.txt").trim()
        part1And2(input).forEach { println(it) }
    }

    private fun part1And2(input: String): Collection<Any> {
        val cross = Stack<Pos>()
        val posToDist = mutableMapOf<Pos, Int>()
        var curPos = Pos(0, 0)
        posToDist[curPos] = 0
        input.substring(1).forEach { c ->
            if (c == '(') {
                cross.push(curPos)
            } else if (c == '$') {
                // do nth
            } else if (c == ')') {
                curPos = cross.pop()
            } else if (c == '|') {
                curPos = cross.peek()
            } else {
                val distance = posToDist[curPos]!!
                curPos = when (c) {
                    'E' -> curPos.goE()
                    'W' -> curPos.goW()
                    'N' -> curPos.goN()
                    'S' -> curPos.goS()
                    else -> throw RuntimeException()
                }
                if (posToDist[curPos] != null) {
                    if (posToDist[curPos]!! > distance + 1) {
                        posToDist[curPos] = distance + 1
                    }
                } else {
                    posToDist[curPos] = distance + 1
                }
            }
        }

        return listOf(
            posToDist.values.maxOrNull()!!,
            posToDist.values.count { it >= 1000 }
        )

    }

    data class Pos(val x: Int, val y: Int) {
        fun goE(): Pos {
            return Pos(x + 1, y)
        }

        fun goW(): Pos {
            return Pos(x - 1, y)
        }

        fun goN(): Pos {
            return Pos(x, y - 1)
        }

        fun goS(): Pos {
            return Pos(x, y + 1)
        }
    }
}
