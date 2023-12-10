package dpr.aoc2023

import java.util.PriorityQueue

object Day10 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/10/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val mapa = mutableMapOf<Point2D, Sign>()
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                mapa[Point2D(x, y)] = Sign.from(c)
            }
        }
        val start = mapa.filter { it.value == Sign.S }.keys.single()
        val realStartSign = detectRealStartSign(mapa, start)
        mapa[start] = realStartSign

        var max = 0L
        val visited = mutableSetOf<Point2D>()
        val toVisit = PriorityQueue<CurPos>()
        toVisit.add(CurPos(start, 0))
        while (toVisit.isNotEmpty()) {
            val cur = toVisit.poll()
            if (cur.p in visited) {
                continue
            }
            visited.add(cur.p)
            if (cur.steps > max) {
                max = cur.steps
            }
            val sign = mapa[cur.p]!!
            if (sign.canUp()) {
                toVisit.offer(CurPos(cur.p.up(), cur.steps + 1))
            }
            if (sign.canDown()) {
                toVisit.offer(CurPos(cur.p.down(), cur.steps + 1))
            }
            if (sign.canLeft()) {
                toVisit.offer(CurPos(cur.p.left(), cur.steps + 1))
            }
            if (sign.canRight()) {
                toVisit.offer(CurPos(cur.p.right(), cur.steps + 1))
            }
        }
        return max
    }

    private fun detectRealStartSign(mapa: MutableMap<Point2D, Sign>, start: Point2D): Sign {
        val possibleStartSign = mutableSetOf(Sign.L, Sign.J, Sign.UP_DOWN, Sign.MINUS, Sign._7, Sign.F)
        println(possibleStartSign)
        if (mapa[start.up()]!!.canDown()) {
            possibleStartSign.removeAll(possibleStartSign.filter { !it.canUp() })
        } else {
            possibleStartSign.removeAll(possibleStartSign.filter { it.canUp() })
        }
        println(possibleStartSign)
        if (mapa[start.down()]!!.canUp()) {
            possibleStartSign.removeAll(possibleStartSign.filter { !it.canDown() })
        } else {
            possibleStartSign.removeAll(possibleStartSign.filter { it.canDown() })
        }
        println(possibleStartSign)
        // TODO check other directions to be generic
        val realStartSign = possibleStartSign.single()
        return realStartSign
    }

    enum class Sign {
        _7,
        L,
        F,
        J,
        MINUS,
        UP_DOWN,
        S,
        DOT;

        fun canUp() = this in setOf(L, J, UP_DOWN)
        fun canDown() = this in setOf(F, _7, UP_DOWN)

        fun canLeft() = this in setOf(_7, J, MINUS)
        fun canRight() = this in setOf(F, L, MINUS)

        companion object {
            fun from(c: Char): Sign = when (c) {
                '7' -> _7
                'L' -> L
                'F' -> F
                'J' -> J
                '|' -> UP_DOWN
                '-' -> MINUS
                '.' -> DOT
                'S' -> S
                else -> throw RuntimeException("Unknown $c")
            }
        }

    }

    data class CurPos(val p: Point2D, val steps: Long = 0L) : Comparable<CurPos> {
        override fun compareTo(other: CurPos): Int {
            return steps.compareTo(other.steps)
        }
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

