package dpr.aoc2021

import dpr.commons.Util
import java.util.PriorityQueue
import kotlin.math.absoluteValue

object Day15 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/15/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val numbers = lines.map { line -> line.map { it.toString().toInt() } }
        val init = Pos(0, 0)
        val pq = PriorityQueue<Path>()
        val maxX = numbers[0].indices.last
        val maxY = numbers.indices.last
        val dest = Pos(maxX, maxY)
        pq.offer(Path(init, 0, dest))
        var maxRisk = -1
        val visited = mutableSetOf<Pos>()
        while (!pq.isEmpty()) {
            val cur = pq.poll()
            if (cur.pos == dest) {
                return cur.risk
            }
            visited.add(cur.pos)
            if (cur.risk > maxRisk) {
                maxRisk = cur.risk
//                println("Current risk is ${cur.risk}")
            }
            neigh(cur.pos.x, cur.pos.y)
                .filter { it.x in (0..maxX) && it.y in (0..maxY) }
                .filter { it !in visited }
                .forEach {
                    pq.offer(Path(it, cur.risk + numbers[it.y][it.x], dest))
                }
        }
        return -1
    }

    data class Pos(val x: Int, val y: Int) {
        fun manhattan(dest: Pos): Int {
            return (dest.x - x).absoluteValue + (dest.y - y).absoluteValue
        }
    }

    data class Path(val pos: Pos, val risk: Int = 0, val dest: Pos) : Comparable<Path> {
        override fun compareTo(other: Path): Int {
            return (risk + pos.manhattan(dest)).compareTo((other.risk + other.pos.manhattan(dest)))
        }
    }

    private fun neigh(i: Int, j: Int): Set<Pos> {
        return setOf(
            Pos(i, j + 1),
            Pos(i, j - 1),
            Pos(i - 1, j),
            Pos(i + 1, j),
        )
    }

    private fun part2(lines: List<String>): Any {
        val board = Board(lines.map { line -> line.map { it.toString().toInt() } })
        val init = Pos(0, 0)
        val pq = PriorityQueue<Path>()
        val maxX = lines[0].length * 5 - 1
        val maxY = lines.size * 5 - 1
        val dest = Pos(maxX, maxY)
        pq.offer(Path(init, 0, dest))
        var maxRisk = -1
        val visited = mutableSetOf<Pos>()
        while (!pq.isEmpty()) {
            val cur = pq.poll()
            if (cur.pos == dest) {
                return cur.risk
            }
            visited.add(cur.pos)
            if (cur.risk > maxRisk) {
                maxRisk = cur.risk
//                println("Current risk is ${cur.risk}")
            }
            neigh(cur.pos.x, cur.pos.y)
                .filter { it.x in (0..maxX) && it.y in (0..maxY) }
                .filter { it !in visited }
                .forEach {
                    pq.offer(Path(it, cur.risk + board.valueOn(it), dest))
                }
        }
        return -1
    }

    data class Board(val init: List<List<Int>>) {
        fun valueOn(pos: Pos): Int {
            val initX = pos.x % init.size
            val rX = pos.x / init.size
            val initY = pos.y % init.size
            val rY = pos.y / init.size
            val normal = init[initY][initX] + rX + rY
            return if (normal >= 10) normal % 10 + 1 else normal
        }
    }
}


