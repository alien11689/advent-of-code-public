package dpr.aoc2018

import dpr.commons.Util
import java.util.Stack
import dpr.commons.Point2D as Pos

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
                    'E' -> curPos.right()
                    'W' -> curPos.left()
                    'N' -> curPos.up()
                    'S' -> curPos.down()
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
}
