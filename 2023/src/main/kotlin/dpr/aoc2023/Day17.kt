package dpr.aoc2023

import java.util.PriorityQueue

object Day17 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/17/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/17/test1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    data class Current(val pos: Point2D, val heat: Int, val dir: Dir?, val prevDirs: List<Dir>, val maxX: Int, val maxY: Int) : Comparable<Current> {

        private val score = heat + maxX + maxY - pos.x - pos.y
        val lastPrevDirs = prevDirs.reversed().take(3)
        val exhaustedDirection = if (lastPrevDirs.size == 3 && lastPrevDirs.toSet().size == 1) lastPrevDirs.first() else null
        override fun compareTo(other: Current): Int = score.compareTo(other.score)
        fun nextMoves(board: Map<Point2D, Int>): List<Current> {
            val dirs = when {
                dir == null -> listOf(Dir.E, Dir.S)
                exhaustedDirection != null -> listOf(dir.turnLeft(), dir.turnRight())
                else -> listOf(dir.turnLeft(), dir, dir.turnRight())
            }
            return dirs.mapNotNull { nextDir ->
                val nextPos = pos.move(nextDir)
                val newHeat = board[nextPos]
                if (newHeat == null) {
                    null
                } else {
                    this.copy(pos = nextPos, heat = heat + newHeat, dir = nextDir, prevDirs = prevDirs + nextDir)
                }
            }
        }

    }

    private fun part1(lines: List<String>): Any {
        val board = mutableMapOf<Point2D, Int>()
        var maxX = 0
        var maxY = 0
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                board[Point2D(x, y)] = c.digitToInt()
                maxX = x
            }
            maxY = y
        }
        val pq = PriorityQueue<Current>()
        pq.offer(Current(Point2D(0, 0), 0, null, emptyList(), maxX, maxY))
        val mem = mutableSetOf<Pair<Point2D, List<Dir>>>()
        val target = Point2D(maxX, maxY)
        while (pq.isNotEmpty()) {
            val cur = pq.poll()
//            println("Checking ${cur.heat}")
            if (cur.pos == target) {
//                println("Found $cur")
//                continue
                return cur.heat
            }
            val memItem = cur.pos to cur.lastPrevDirs
            if (memItem in mem) {
                continue
            }
            mem.add(memItem)
            cur.nextMoves(board).forEach { pq.offer(it) }
        }
        throw RuntimeException()
        // 984 is too high
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

