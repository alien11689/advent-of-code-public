package dpr.aoc2022

import dpr.commons.Util
import java.util.PriorityQueue
import dpr.commons.Point2D as Point

object Day24 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/24/input.txt")
//        part1And2(Util.getNotEmptyLinesFromFile("/24/test1.txt"))
        part1And2(lines)
    }

    private fun part1And2(lines: List<String>) {
        val board = parseBoard(lines)
        val minY = board.minOf { it.key.y }
        val maxY = board.maxOf { it.key.y }
        val minX = board.minOf { it.key.x }
        val maxX = board.maxOf { it.key.x }
        val start = board.filter { it.key.y == minY && it.value == listOf(Wind.EMPTY) }.keys.first()
        val target = board.filter { it.key.y == maxY && it.value == listOf(Wind.EMPTY) }.keys.first()
        val boards = mutableMapOf<Int, Map<Point, List<Wind>>>()
        boards[0] = board.filter { it.value != listOf(Wind.EMPTY) }
        val yRange = minY..maxY
        val xRange = minX..maxX

        val journeyToTarget = traverse(0, start, target, boards, xRange, yRange)
        println(journeyToTarget) // Part 1
        val journeyToStart = traverse(journeyToTarget, target, start, boards, xRange, yRange)
        println(traverse(journeyToStart, start, target, boards, xRange, yRange)) // Part 2
    }

    enum class Wind {
        UP,
        DOWN,
        LEFT,
        RIGHT,
        WALL,
        EMPTY
    }

    private fun traverse(initTime: Int, start: Point, target: Point, boards: MutableMap<Int, Map<Point, List<Wind>>>, xRange: IntRange, yRange: IntRange): Int {
        val visited = mutableSetOf<State>()
        val pq = PriorityQueue<State>()
        pq.offer(State(start, initTime, start.manhattan(target)))
        while (pq.isNotEmpty()) {
            val cur = pq.poll()
            if (cur in visited) {
                continue
            }
            visited.add(cur)
            val nextTime = cur.time + 1
            val newBoard = if (nextTime in boards) boards[nextTime]!! else {
                val b = generateNextBoard(boards[cur.time]!!, xRange, yRange)
                boards[nextTime] = b
                b
            }
            (cur.curPos.neighboursCross() + cur.curPos)
                .filter { it.y in yRange }
                .forEach { nextPoint ->
                    if (nextPoint !in newBoard) {
                        val nextState = cur.copy(curPos = nextPoint, time = nextTime, distanceToTarget = nextPoint.manhattan(target))
                        if (nextState.curPos == target) {
                            return nextTime
                        }
                        pq.offer(nextState)
                    }
                }
        }
        return Int.MIN_VALUE
    }

    private fun generateNextBoard(current: Map<Point, List<Wind>>, xRange: IntRange, yRange: IntRange): Map<Point, List<Wind>> {
        val board = mutableMapOf<Point, List<Wind>>()
        val minY = yRange.min()
        val maxY = yRange.max()
        val minX = xRange.min()
        val maxX = xRange.max()
        current.forEach { (point, winds) ->
            if (winds == listOf(Wind.WALL)) {
                board[point] = winds
            } else {
                winds.forEach { curWind ->
                    val next = when (curWind) {
                        Wind.LEFT -> point.copy(x = point.x - 1).let { if (it.x != minX) it else point.copy(x = maxX - 1) }
                        Wind.RIGHT -> point.copy(x = point.x + 1).let { if (it.x != maxX) it else point.copy(x = minX + 1) }
                        Wind.UP -> point.copy(y = point.y - 1).let { if (it.y != minY) it else point.copy(y = maxY - 1) }
                        Wind.DOWN -> point.copy(y = point.y + 1).let { if (it.y != maxY) it else point.copy(y = minY + 1) }
                        else -> throw RuntimeException("Unknown wind $curWind")
                    }
                    board[next] = (board[next] ?: emptyList()) + curWind
                }
            }
        }
        return board.toMap()
    }


    data class State(val curPos: Point, val time: Int, val distanceToTarget: Int) : Comparable<State> {
        override fun compareTo(other: State): Int {
            if (time == other.time) {
                return distanceToTarget.compareTo(other.distanceToTarget)
            }
            return time.compareTo(other.time)
        }

    }

    private fun parseBoard(lines: List<String>): Map<Point, List<Wind>> {
        val board = mutableMapOf<Point, List<Wind>>()
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                when (c) {
                    '#' -> board[Point(x, y)] = listOf(Wind.WALL)
                    '>' -> board[Point(x, y)] = listOf(Wind.RIGHT)
                    '<' -> board[Point(x, y)] = listOf(Wind.LEFT)
                    '^' -> board[Point(x, y)] = listOf(Wind.UP)
                    'v' -> board[Point(x, y)] = listOf(Wind.DOWN)
                    '.' -> board[Point(x, y)] = listOf(Wind.EMPTY)
                    else -> throw RuntimeException("Unknown input $c")
                }
            }
        }
        return board.toMap()
    }

//    }

    //    fun printBoard(current: Map<Point, Set<Wind>>) {
//        val minY = current.minOf { it.key.y }
//        val maxY = current.maxOf { it.key.y }
//        val minX = current.minOf { it.key.x }
//        val maxX = current.maxOf { it.key.x }
//        for (y in minY..maxY) {
//            for (x in minX..maxX) {
//                val p = Point(x, y)
//                when (val winds = current[p]) {
//                    null -> print('.')
//                    setOf(Wind.WALL) -> print("#")
//                    setOf(Wind.UP) -> print("^")
//                    setOf(Wind.DOWN) -> print("v")
//                    setOf(Wind.LEFT) -> print("<")
//                    setOf(Wind.RIGHT) -> print(">")
//                    else -> print(winds.size)
//                }
//            }
//            println()
//        }
}

