package dpr.aoc2023

import java.util.LinkedList
import java.util.PriorityQueue

object Day10 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/10/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val mapa = readMapa(lines)
        val start = mapa.filter { it.value == Sign.S }.keys.single()
        val realStartSign = detectRealStartSign(mapa, start)
        mapa[start] = realStartSign

        val (max, visited) = travel(start, mapa)
        return max
    }

    private fun readMapa(lines: List<String>): MutableMap<Point2D, Sign> {
        val mapa = mutableMapOf<Point2D, Sign>()
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                mapa[Point2D(x, y)] = Sign.from(c)
            }
        }
        return mapa
    }

    private fun travel(
        start: Point2D,
        mapa: MutableMap<Point2D, Sign>
    ): Pair<Long, MutableSet<Point2D>> {
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
        return Pair(max, visited)
    }

    private fun detectRealStartSign(mapa: MutableMap<Point2D, Sign>, start: Point2D): Sign {
        val possibleStartSign = mutableSetOf(Sign.L, Sign.J, Sign.UP_DOWN, Sign.MINUS, Sign._7, Sign.F)
        if (mapa[start.up()]!!.canDown()) {
            possibleStartSign.removeAll(possibleStartSign.filter { !it.canUp() })
        } else {
            possibleStartSign.removeAll(possibleStartSign.filter { it.canUp() })
        }
        if (mapa[start.down()]!!.canUp()) {
            possibleStartSign.removeAll(possibleStartSign.filter { !it.canDown() })
        } else {
            possibleStartSign.removeAll(possibleStartSign.filter { it.canDown() })
        }
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

    enum class Dir {
        UP,
        DOWN,
        LEFT,
        RIGHT;

        fun turn(nextSign: Sign): Dir = when (this) {
            UP -> when (nextSign) {
                Sign._7 -> LEFT
                Sign.F -> RIGHT
                else -> this
            }

            DOWN -> when (nextSign) {
                Sign.J -> LEFT
                Sign.L -> RIGHT
                else -> this
            }

            LEFT -> when (nextSign) {
                Sign.F -> DOWN
                Sign.L -> UP
                else -> this
            }

            RIGHT -> when (nextSign) {
                Sign.J -> UP
                Sign._7 -> DOWN
                else -> this
            }
        }
    }

    private fun part2(lines: List<String>): Any {
        val mapa = readMapa(lines)
        val start = mapa.filter { it.value == Sign.S }.keys.single()
        val realStartSign = detectRealStartSign(mapa, start)
        mapa[start] = realStartSign

        val visited = mutableSetOf<Point2D>()
        var cur = start
        var curDir = Dir.RIGHT // assume we are minus on start
        var curInteriorDir = setOf(Dir.UP) // assume interior is above
        val possibleInterior = mutableSetOf<Point2D>()
        possibleInterior.add(cur.up())
        while (true) {
            visited.add(cur)
            val next = when (curDir) {
                Dir.UP -> cur.up()
                Dir.DOWN -> cur.down()
                Dir.LEFT -> cur.left()
                Dir.RIGHT -> cur.right()
            }
            if (next in visited) {
                break
            }
            val nextSign = mapa[next]!!
            val nextDir = curDir.turn(nextSign)
            curInteriorDir = when {
                nextSign == Sign.MINUS -> curInteriorDir.filter { it in setOf(Dir.UP, Dir.DOWN) }.toSet()
                nextSign == Sign.UP_DOWN -> curInteriorDir.filter { it in setOf(Dir.UP, Dir.DOWN) }.toSet()
                curDir == Dir.RIGHT && nextSign == Sign._7 -> if (Dir.UP in curInteriorDir) setOf(Dir.UP, Dir.RIGHT) else setOf(Dir.DOWN, Dir.LEFT)
                curDir == Dir.UP && nextSign == Sign._7 -> if (Dir.RIGHT in curInteriorDir) setOf(Dir.UP, Dir.RIGHT) else setOf(Dir.DOWN, Dir.LEFT)
                curDir == Dir.RIGHT && nextSign == Sign.J -> if (Dir.UP in curInteriorDir) setOf(Dir.UP, Dir.LEFT) else setOf(Dir.DOWN, Dir.RIGHT)
                curDir == Dir.DOWN && nextSign == Sign.J -> if (Dir.LEFT in curInteriorDir) setOf(Dir.UP, Dir.LEFT) else setOf(Dir.DOWN, Dir.RIGHT)
                curDir == Dir.DOWN && nextSign == Sign.L -> if (Dir.LEFT in curInteriorDir) setOf(Dir.DOWN, Dir.LEFT) else setOf(Dir.UP, Dir.RIGHT)
                curDir == Dir.LEFT && nextSign == Sign.L -> if (Dir.DOWN in curInteriorDir) setOf(Dir.DOWN, Dir.LEFT) else setOf(Dir.UP, Dir.RIGHT)
                curDir == Dir.UP && nextSign == Sign.F -> if (Dir.RIGHT in curInteriorDir) setOf(Dir.DOWN, Dir.RIGHT) else setOf(Dir.UP, Dir.LEFT)
                curDir == Dir.LEFT && nextSign == Sign.F -> if (Dir.DOWN in curInteriorDir) setOf(Dir.DOWN, Dir.RIGHT) else setOf(Dir.UP, Dir.LEFT)
                else -> throw RuntimeException()
            }
            cur = next
            curDir = nextDir
            curInteriorDir.forEach { dir ->
                val posInterior = when (dir) {
                    Dir.UP -> cur.up()
                    Dir.DOWN -> cur.down()
                    Dir.LEFT -> cur.left()
                    Dir.RIGHT -> cur.right()
                }
                possibleInterior.add(posInterior)
            }
        }
        println("Possible after road: " + possibleInterior.size)
        possibleInterior.removeAll(visited)
        println("Possible without visited: " + possibleInterior.size)
        val knownInterior = mutableSetOf<Point2D>()
        for (dot in possibleInterior) {
            if (dot in knownInterior) {
                continue
            }
            val localKnownEmpty = LinkedList<Point2D>()
            localKnownEmpty.add(dot)
            val localVisited = mutableSetOf<Point2D>()
            while (localKnownEmpty.isNotEmpty()) {
                val current = localKnownEmpty.pop()
                if (current in localVisited) {
                    continue
                }
                if(cur !in mapa){
                    throw RuntimeException("Bla")
                }
                localVisited.add(current)
                current.adjacentPoints().filter { it !in visited }.forEach { localKnownEmpty.push(it) }
            }
            knownInterior.addAll(localVisited)
        }
        // 518 is too high
        // 28 is wrong
        // 13904 is too high
        // 306 is wrong
        return knownInterior.size
    }
}

