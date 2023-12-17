package dpr.aoc2016

import dpr.commons.Util
import java.util.LinkedList

object Day13 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = 1350
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: Int): Any {
        val s = Stage(1, 1, 0)
        val q = LinkedList<Stage>()
        val mem = mutableSetOf<Pair<Int, Int>>()
        mem.add(s.point())
        q.add(s)
        while (q.isNotEmpty()) {
            val cur = q.poll()
            mem.add(cur.point())
            if (cur.x == 31 && cur.y == 39) {
                return cur.step
            }
            q.addAll(cur.neighbours(input).filter { it.point() !in mem })
        }
        throw RuntimeException()
    }

    private fun part2(input: Int): Any {
        val s = Stage(1, 1, 0)
        val q = LinkedList<Stage>()
        val mem = mutableSetOf<Pair<Int, Int>>()
        mem.add(s.point())
        q.add(s)
        while (q.isNotEmpty()) {
            val cur = q.poll()
            if (cur.step > 50) {
                return mem.size
            }
            mem.add(cur.point())
            q.addAll(cur.neighbours(input).filter { it.point() !in mem })
        }
        throw RuntimeException()
    }

    private fun isOpenSpace(x: Int, y: Int, magic: Int): Boolean =
        Integer.toBinaryString(x * x + 3 * x + 2 * x * y + y + y * y + magic).count { it == '1' } % 2 == 0

    data class Stage(val x: Int, val y: Int, val step: Int) {
        fun neighbours(magic: Int): List<Stage> {
            return listOf(
                Pair(x, y + 1),
                Pair(x, y - 1),
                Pair(x + 1, y),
                Pair(x - 1, y),
            )
                .filter { it.first >= 0 && it.second >= 0 }
                .filter { isOpenSpace(it.first, it.second, magic) }
                .map { Stage(it.first, it.second, step + 1) }

        }

        fun point(): Pair<Int, Int> = Pair(x, y)
    }
}
