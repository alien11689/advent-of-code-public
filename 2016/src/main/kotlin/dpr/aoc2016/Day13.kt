package dpr.aoc2016

import dpr.commons.Point2D
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
        val s = Stage(Point2D(1, 1), 0)
        val q = LinkedList<Stage>()
        val mem = mutableSetOf<Point2D>()
        mem.add(s.point)
        q.add(s)
        while (q.isNotEmpty()) {
            val cur = q.poll()
            mem.add(cur.point)
            if (cur.point.x == 31 && cur.point.y == 39) {
                return cur.step
            }
            q.addAll(cur.neighbours(input).filter { it.point !in mem })
        }
        throw RuntimeException()
    }

    private fun part2(input: Int): Any {
        val s = Stage(Point2D(1, 1), 0)
        val q = LinkedList<Stage>()
        val mem = mutableSetOf<Point2D>()
        mem.add(s.point)
        q.add(s)
        while (q.isNotEmpty()) {
            val cur = q.poll()
            if (cur.step > 50) {
                return mem.size
            }
            mem.add(cur.point)
            q.addAll(cur.neighbours(input).filter { it.point !in mem })
        }
        throw RuntimeException()
    }

    private fun isOpenSpace(x: Int, y: Int, magic: Int): Boolean =
        Integer.toBinaryString(x * x + 3 * x + 2 * x * y + y + y * y + magic).count { it == '1' } % 2 == 0

    data class Stage(val point: Point2D, val step: Int) {
        fun neighbours(magic: Int): List<Stage> {
            return point.neighboursCross()
                .filter { it.x >= 0 && it.y >= 0 }
                .filter { isOpenSpace(it.x, it.y, magic) }
                .map { Stage(it, step + 1) }

        }
    }
}
