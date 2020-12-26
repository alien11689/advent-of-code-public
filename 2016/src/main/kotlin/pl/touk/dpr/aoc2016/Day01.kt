package pl.touk.dpr.aoc2016

import kotlin.math.absoluteValue

object Day01 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getFileContent("/01/input.txt").trim().split(Regex("[ ,]+"))
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        return input.fold(Position(0, 0)) { acc, c -> acc.go(c) }.manhattan()
    }

    private fun part2(input: List<String>): Any {
        val mem = mutableSetOf<Pair<Int, Int>>()
        var cur = Position(0, 0)
        mem.add(cur.point())
        input.forEach {
            val turn = it.first()
            var step = it.drop(1).toInt() - 1
            cur = cur.go(turn + "1")
            if (cur.point() in mem) {
                return cur.manhattan()
            }
            mem.add(cur.point())
            while (step > 0) {
                cur = cur.go("C1")
                if (cur.point() in mem) {
                    return cur.manhattan()
                }
                mem.add(cur.point())
                --step
            }
        }
        throw RuntimeException()
    }

    data class Position(val x: Int, val y: Int, val dir: Direction = Direction.N) {
        fun go(input: String): Position {
            val nextDir = when (input.first()) {
                'L' -> dir.left()
                'R' -> dir.right()
                else -> dir
            }
            val step = input.drop(1).toInt()
            return Position(dir = nextDir, x = x + nextDir.dx * step, y = y + nextDir.dy * step)
        }

        fun manhattan(): Int = x.absoluteValue + y.absoluteValue

        fun point(): Pair<Int, Int> = Pair(x, y)
    }

    enum class Direction(val dx: Int, val dy: Int) {
        N(0, -1),
        S(0, 1),
        W(-1, 0),
        E(1, 0);

        fun left(): Direction = when (this) {
            N -> W
            S -> E
            W -> S
            E -> N
        }

        fun right(): Direction = when (this) {
            N -> E
            S -> W
            W -> N
            E -> S
        }
    }
}

