package dpr.aoc2023

import dpr.commons.Dir
import dpr.commons.Point2D
import dpr.commons.Util
import java.util.PriorityQueue

object Day17 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/17/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/17/test1.txt")
        val (board, target) = readBoard(lines)
        println(part1(board, target))
        println(part2(board, target))
    }

    data class Current(val pos: Point2D, val heat: Int, val dir: Dir) : Comparable<Current> {

        private val score = heat - pos.x - pos.y
        override fun compareTo(other: Current): Int = score.compareTo(other.score)
        fun nextMoves(board: Map<Point2D, Int>, minSteps: Int, maxSteps: Int): List<Current> {
            var i = 0
            val res = mutableListOf<Current>()
            var newHeat = 0
            while (i < maxSteps) {
                ++i
                val newPos = pos.move(dir, i)
                val curHeat = board[newPos] ?: break
                newHeat += curHeat
                if (i >= minSteps) {
                    res.add(copy(pos = newPos, heat = heat + newHeat, dir = dir.turnLeft()))
                    res.add(copy(pos = newPos, heat = heat + newHeat, dir = dir.turnRight()))
                }
            }
//            println("From $this to $res")
            return res
        }

    }

    private fun part1(board: Map<Point2D, Int>, target: Point2D): Any {
        return traverse(board, target, 1, 3)
    }

    private fun part2(board: Map<Point2D, Int>, target: Point2D): Any {
        return traverse(board, target, 4, 10)
    }

    private fun readBoard(lines: List<String>): Pair<Map<Point2D, Int>, Point2D> {
        val board = Util.readBoard(lines) { it.digitToInt() }
        return Pair(board, Point2D(lines[0].length - 1, lines.size - 1))
    }

    private fun traverse(board: Map<Point2D, Int>, target: Point2D, minSteps: Int, maxSteps: Int): Int {
        val pq = PriorityQueue<Current>()
        pq.offer(Current(Point2D(0, 0), 0, Dir.E))
        pq.offer(Current(Point2D(0, 0), 0, Dir.S))
        val mem = mutableSetOf<Pair<Point2D, Dir>>()
        while (pq.isNotEmpty()) {
            val cur = pq.poll()
            if (cur.pos == target) {
                return cur.heat
            }
            val memItem = cur.pos to cur.dir
            if (memItem in mem) {
                continue
            }
            mem.add(memItem)
            cur.nextMoves(board, minSteps, maxSteps).forEach { pq.offer(it) }
        }
        throw RuntimeException()
    }
}

