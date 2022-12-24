package pl.touk.dpr.aoc2022

import java.util.PriorityQueue
import kotlin.math.absoluteValue

object Day24 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/24/input.txt")
        println("Part 1:")
        println(part1(Util.getNotEmptyLinesFromFile("/24/test1.txt")))
        println(part1(lines))
//        println("Part 2:")
//        println(part2(Util.getNotEmptyLinesFromFile("/24/test1.txt")))
//        println(part2(lines))
        // 382 is too high
    }

    data class Point(val x: Int, val y: Int) {
        fun manhattan(dest: Point): Int {
            return (dest.x - x).absoluteValue + (dest.y - y).absoluteValue
        }

        fun nextPositions() = setOf(
            copy(x = x - 1),
            copy(x = x + 1),
            copy(y = y - 1),
            copy(y = y + 1),
            this,
        )
    }

    enum class Wind {
        UP,
        DOWN,
        LEFT,
        RIGHT,
        WALL,
        EMPTY
    }

    private fun part1(lines: List<String>): Any {
        val board = parseBoard(lines)
        val minY = board.minOf { it.key.y }
        val maxY = board.maxOf { it.key.y }
        val start = board.filter { it.key.y == minY && it.value == setOf(Wind.EMPTY) }.keys.first()
        val target = board.filter { it.key.y == maxY && it.value == setOf(Wind.EMPTY) }.keys.first()
//        println(board)
//        println(start)
//        println(target)

        val boards = mutableMapOf<Int, Map<Point, Set<Wind>>>()
        boards[0] = board.filter { it.value != setOf(Wind.EMPTY) }
        val visited = mutableSetOf<Pair<Point, Int>>()

        val yRange = minY..maxY
        val pq = PriorityQueue<State>()
        pq.offer(State(start, 0, start.manhattan(target)))
        var theBest = Int.MAX_VALUE
        while (pq.isNotEmpty()) {
            val cur = pq.poll()
            if (cur.time >= theBest) {
                continue
            }
            val key = cur.curPos to cur.time
            if (key in visited) {
                continue
            }
            visited.add(key)
//            if (cur.time > maxTime) {
//                maxTime = cur.time
//                println("Checking $maxTime")
//            }
//            println("Checking $cur")
            val nextTime = cur.time + 1
            val newBoard = if (nextTime in boards) boards[nextTime]!! else {
                val b = generateNextBoard(boards[cur.time]!!)
                boards[nextTime] = b
                b
            }
            cur.curPos.nextPositions()
                .filter { it.y in yRange }
                .forEach { nextPoint ->
                    if (nextPoint !in newBoard) {
                        val nextState = cur.copy(curPos = nextPoint, time = nextTime, distanceToTarget = nextPoint.manhattan(target), path = cur.path + (nextPoint to nextTime))
                        if (nextState.curPos == target) {
                            if (nextTime < theBest) {
                                theBest = nextTime
                                println("New Leader $theBest")
                            }
//                            println("Winner $nextState")
//                            for (i in 0..nextTime) {
//                                println("Iteration $i")
//                                printBoard(boards[i]!!)
//                            }
//                            return nextTime
                        }
                        pq.offer(nextState)
                    }
                }
        }
        return theBest
    }

    private fun generateNextBoard(current: Map<Point, Set<Wind>>): Map<Point, Set<Wind>> {
        val board = mutableMapOf<Point, Set<Wind>>()
        val minY = current.minOf { it.key.y }
        val maxY = current.maxOf { it.key.y }
        val minX = current.minOf { it.key.x }
        val maxX = current.maxOf { it.key.x }
        current.forEach { point, winds ->
            if (winds == setOf(Wind.WALL)) {
                board[point] = winds
            } else {
                winds.forEach { curWind ->
                    when (curWind) {
                        Wind.LEFT -> {
                            var next = point.copy(x = point.x - 1)
                            if (next.x == minX) {
                                next = point.copy(x = maxX - 1)
                            }
                            board[next] = (board[next] ?: emptySet()) + curWind
                        }

                        Wind.RIGHT -> {
                            var next = point.copy(x = point.x + 1)
                            if (next.x == maxX) {
                                next = point.copy(x = minX + 1)
                            }
                            board[next] = (board[next] ?: emptySet()) + curWind
                        }

                        Wind.UP -> {
                            var next = point.copy(y = point.y - 1)
                            if (next.y == minY) {
                                next = point.copy(y = maxY - 1)
                            }
                            board[next] = (board[next] ?: emptySet()) + curWind
                        }

                        Wind.DOWN -> {
                            var next = point.copy(y = point.y + 1)
                            if (next.y == maxY) {
                                next = point.copy(y = minY + 1)
                            }
                            board[next] = (board[next] ?: emptySet()) + curWind
                        }

                        else -> throw RuntimeException("Unknown wind $curWind")
                    }
                }
            }
        }
        return board.toMap()
    }


    data class State(val curPos: Point, val time: Int, val distanceToTarget: Int, val path: List<Pair<Point, Int>> = emptyList()) : Comparable<State> {
        override fun compareTo(other: State): Int {
            if (time == other.time) {
                return distanceToTarget.compareTo(other.distanceToTarget)
            }
            return time.compareTo(other.time)
//            if (distanceToTarget == other.distanceToTarget) {
//                return time.compareTo(other.time)
//            }
//            return distanceToTarget.compareTo(other.distanceToTarget)
        }

    }

    private fun parseBoard(lines: List<String>): Map<Point, Set<Wind>> {
        val board = mutableMapOf<Point, Set<Wind>>()
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                when (c) {
                    '#' -> board[Point(x, y)] = setOf(Wind.WALL)
                    '>' -> board[Point(x, y)] = setOf(Wind.RIGHT)
                    '<' -> board[Point(x, y)] = setOf(Wind.LEFT)
                    '^' -> board[Point(x, y)] = setOf(Wind.UP)
                    'v' -> board[Point(x, y)] = setOf(Wind.DOWN)
                    '.' -> board[Point(x, y)] = setOf(Wind.EMPTY)
                    else -> throw RuntimeException("Unknown input $c")
                }
            }
        }
        return board.toMap()
    }

    fun printBoard(current: Map<Point, Set<Wind>>) {
        val minY = current.minOf { it.key.y }
        val maxY = current.maxOf { it.key.y }
        val minX = current.minOf { it.key.x }
        val maxX = current.maxOf { it.key.x }
        for (y in minY..maxY) {
            for (x in minX..maxX) {
                val p = Point(x, y)
                when (val winds = current[p]) {
                    null -> print('.')
                    setOf(Wind.WALL) -> print("#")
                    setOf(Wind.UP) -> print("^")
                    setOf(Wind.DOWN) -> print("v")
                    setOf(Wind.LEFT) -> print("<")
                    setOf(Wind.RIGHT) -> print(">")
                    else -> print(winds.size)
                }
            }
            println()
        }
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

