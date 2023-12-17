package dpr.aoc2023

import dpr.commons.Dir
import dpr.commons.Point2D
import dpr.commons.Util
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
        val board = readBoard(lines)
        val start = board.filter { it.value == Sign.S }.keys.single()
        val realStartSign = detectRealStartSign(board, start)
        board[start] = realStartSign

        var max = 0L
        val visited1 = mutableSetOf<Point2D>()
        val toVisit = PriorityQueue<CurPos>()
        toVisit.add(CurPos(start, 0))
        while (toVisit.isNotEmpty()) {
            val cur = toVisit.poll()
            if (cur.p in visited1) {
                continue
            }
            visited1.add(cur.p)
            if (cur.steps > max) {
                max = cur.steps
            }
            val sign = board[cur.p]!!
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
        return max
    }

    private fun readBoard(lines: List<String>): MutableMap<Point2D, Sign> {
        val board = mutableMapOf<Point2D, Sign>()
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                board[Point2D(x, y)] = Sign.from(c)
            }
        }
        return board
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
        if (mapa[start.left()]!!.canRight()) {
            possibleStartSign.removeAll(possibleStartSign.filter { !it.canLeft() })
        } else {
            possibleStartSign.removeAll(possibleStartSign.filter { it.canLeft() })
        }
        if (mapa[start.right()]!!.canLeft()) {
            possibleStartSign.removeAll(possibleStartSign.filter { !it.canRight() })
        } else {
            possibleStartSign.removeAll(possibleStartSign.filter { it.canRight() })
        }
        return possibleStartSign.single()
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
        fun directions(): List<Dir> = when (this) {
            _7 -> listOf(Dir.W, Dir.N)
            L -> listOf(Dir.S, Dir.E)
            F -> listOf(Dir.N, Dir.E)
            J -> listOf(Dir.S, Dir.W)
            MINUS -> listOf(Dir.W, Dir.E)
            UP_DOWN -> listOf(Dir.S, Dir.N)
            S, DOT -> throw RuntimeException()
        }

        fun possibleInteriorDirectionSelector(): Set<Dir> = when (this) {
            _7, L -> setOf(Dir.S, Dir.E)
            F, J -> setOf(Dir.S, Dir.W)
            MINUS -> setOf(Dir.S)
            UP_DOWN -> setOf(Dir.W)
            S, DOT -> throw RuntimeException()
        }

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

    private fun part2(lines: List<String>): Any {
        val board = readBoard(lines)
        val start = board.filter { it.value == Sign.S }.keys.single()
        val realStartSign = detectRealStartSign(board, start)
        board[start] = realStartSign

        val visited = mutableSetOf<Point2D>()
        var cur = start
        var curDir = realStartSign.directions().first()
        var interiorDirectionSelector = realStartSign.possibleInteriorDirectionSelector()
        val possibleInterior1 = mutableSetOf<Point2D>()
        val possibleInterior2 = mutableSetOf<Point2D>()
        possibleInterior1.add(cur.up())
        possibleInterior2.add(cur.down())
        while (true) {
            visited.add(cur)
            val next = when (curDir) {
                Dir.S -> cur.up()
                Dir.N -> cur.down()
                Dir.W -> cur.left()
                Dir.E -> cur.right()
            }
            if (next in visited) {
                break
            }
            val nextSign = board[next]!!
            val nextDir = curDir.turn(nextSign)
            interiorDirectionSelector = when {
                nextSign == Sign.MINUS -> interiorDirectionSelector.filter { it in setOf(Dir.S, Dir.N) }.toSet()
                nextSign == Sign.UP_DOWN -> interiorDirectionSelector.filter { it in setOf(Dir.W, Dir.E) }.toSet()
                curDir == Dir.E && nextSign == Sign._7 -> if (Dir.S in interiorDirectionSelector) setOf(Dir.S, Dir.E) else setOf(Dir.N, Dir.W)
                curDir == Dir.S && nextSign == Sign._7 -> if (Dir.E in interiorDirectionSelector) setOf(Dir.S, Dir.E) else setOf(Dir.N, Dir.W)
                curDir == Dir.E && nextSign == Sign.J -> if (Dir.S in interiorDirectionSelector) setOf(Dir.S, Dir.W) else setOf(Dir.N, Dir.E)
                curDir == Dir.N && nextSign == Sign.J -> if (Dir.W in interiorDirectionSelector) setOf(Dir.S, Dir.W) else setOf(Dir.N, Dir.E)
                curDir == Dir.N && nextSign == Sign.L -> if (Dir.W in interiorDirectionSelector) setOf(Dir.N, Dir.W) else setOf(Dir.S, Dir.E)
                curDir == Dir.W && nextSign == Sign.L -> if (Dir.N in interiorDirectionSelector) setOf(Dir.N, Dir.W) else setOf(Dir.S, Dir.E)
                curDir == Dir.S && nextSign == Sign.F -> if (Dir.E in interiorDirectionSelector) setOf(Dir.N, Dir.E) else setOf(Dir.S, Dir.W)
                curDir == Dir.W && nextSign == Sign.F -> if (Dir.N in interiorDirectionSelector) setOf(Dir.N, Dir.E) else setOf(Dir.S, Dir.W)
                else -> throw RuntimeException()
            }
            cur = next
            curDir = nextDir
            interiorDirectionSelector.forEach { dir ->
                possibleInterior1.add(
                    when (dir) {
                        Dir.S -> cur.up()
                        Dir.N -> cur.down()
                        Dir.W -> cur.left()
                        Dir.E -> cur.right()
                    }
                )
                possibleInterior2.add(
                    when (dir.opposite()) {
                        Dir.S -> cur.up()
                        Dir.N -> cur.down()
                        Dir.W -> cur.left()
                        Dir.E -> cur.right()
                    }
                )
            }
        }
        possibleInterior1.removeAll(visited)
        possibleInterior2.removeAll(visited)
        val possibleInterior = listOf(possibleInterior1, possibleInterior2).single { candidate -> !candidate.any { it !in board } }
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
                localVisited.add(current)
                current.adjacentPoints().filter { it !in visited }.forEach { localKnownEmpty.push(it) }
            }
            knownInterior.addAll(localVisited)
        }
//        lines.forEachIndexed { y, line ->
//            line.forEachIndexed { x, c ->
//                if (Point2D(x, y) in visited) {
//                    print('_')
//                } else if (Point2D(x, y) in knownInterior) {
//                    print('#')
//                } else {
//                    print(' ')
//                }
//            }
//            println()
//        }
        // 518 is too high
        // 28 is wrong
        // 13904 is too high
        // 306 is wrong
        return knownInterior.size
    }

    fun Dir.turn(nextSign: Sign): Dir = when (this) {
        Dir.S -> when (nextSign) {
            Sign._7 -> Dir.W
            Sign.F -> Dir.E
            else -> this
        }

        Dir.N -> when (nextSign) {
            Sign.J -> Dir.W
            Sign.L -> Dir.E
            else -> this
        }

        Dir.W -> when (nextSign) {
            Sign.F -> Dir.N
            Sign.L -> Dir.S
            else -> this
        }

        Dir.E -> when (nextSign) {
            Sign.J -> Dir.S
            Sign._7 -> Dir.N
            else -> this
        }
    }
}

