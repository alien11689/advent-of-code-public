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
        val (board, maxX, maxY) = readBoard(lines)
        println(part1(board, maxX, maxY))
        println(part2(board, maxX, maxY))
    }

    data class Current(val pos: Point2D, val heat: Int, val dir: Dir, val maxX: Int, val maxY: Int) : Comparable<Current> {

        private val score = heat + maxX + maxY - pos.x - pos.y
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

    private fun part1(board: MutableMap<Point2D, Int>, maxX: Int, maxY: Int): Any {
        return traverse(board, maxX, maxY, 1, 3)
    }

    private fun part2(board: MutableMap<Point2D, Int>, maxX: Int, maxY: Int): Any {
        return traverse(board, maxX, maxY, 4, 10)
    }

    private fun readBoard(lines: List<String>): Triple<MutableMap<Point2D, Int>, Int, Int> {
        val board = mutableMapOf<Point2D, Int>()
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                board[Point2D(x, y)] = c.digitToInt()
            }
        }
        return Triple(board, lines[0].length - 1, lines.size - 1)
    }

    private fun traverse(board: MutableMap<Point2D, Int>, maxX: Int, maxY: Int, minSteps: Int, maxSteps: Int): Int {
        val pq = PriorityQueue<Current>()
        pq.offer(Current(Point2D(0, 0), 0, Dir.E, maxX, maxY))
        pq.offer(Current(Point2D(0, 0), 0, Dir.S, maxX, maxY))
        val mem = mutableSetOf<Pair<Point2D, Dir>>()
        val target = Point2D(maxX, maxY)
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

