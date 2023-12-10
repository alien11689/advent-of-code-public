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
            _7 -> listOf(Dir.LEFT, Dir.DOWN)
            L -> listOf(Dir.UP, Dir.RIGHT)
            F -> listOf(Dir.DOWN, Dir.RIGHT)
            J -> listOf(Dir.UP, Dir.LEFT)
            MINUS -> listOf(Dir.LEFT, Dir.RIGHT)
            UP_DOWN -> listOf(Dir.UP, Dir.DOWN)
            S, DOT -> throw RuntimeException()
        }

        fun possibleInteriorDirectionSelector(): Set<Dir> = when(this){
            _7, L -> setOf(Dir.UP, Dir.RIGHT)
            F, J -> setOf(Dir.UP, Dir.LEFT)
            MINUS -> setOf(Dir.UP)
            UP_DOWN -> setOf(Dir.LEFT)
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

        fun opposite(): Dir = when (this) {
            UP -> DOWN
            DOWN -> UP
            LEFT -> RIGHT
            RIGHT -> LEFT
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
                Dir.UP -> cur.up()
                Dir.DOWN -> cur.down()
                Dir.LEFT -> cur.left()
                Dir.RIGHT -> cur.right()
            }
            if (next in visited) {
                break
            }
            val nextSign = board[next]!!
            val nextDir = curDir.turn(nextSign)
            interiorDirectionSelector = when {
                nextSign == Sign.MINUS -> interiorDirectionSelector.filter { it in setOf(Dir.UP, Dir.DOWN) }.toSet()
                nextSign == Sign.UP_DOWN -> interiorDirectionSelector.filter { it in setOf(Dir.LEFT, Dir.RIGHT) }.toSet()
                curDir == Dir.RIGHT && nextSign == Sign._7 -> if (Dir.UP in interiorDirectionSelector) setOf(Dir.UP, Dir.RIGHT) else setOf(Dir.DOWN, Dir.LEFT)
                curDir == Dir.UP && nextSign == Sign._7 -> if (Dir.RIGHT in interiorDirectionSelector) setOf(Dir.UP, Dir.RIGHT) else setOf(Dir.DOWN, Dir.LEFT)
                curDir == Dir.RIGHT && nextSign == Sign.J -> if (Dir.UP in interiorDirectionSelector) setOf(Dir.UP, Dir.LEFT) else setOf(Dir.DOWN, Dir.RIGHT)
                curDir == Dir.DOWN && nextSign == Sign.J -> if (Dir.LEFT in interiorDirectionSelector) setOf(Dir.UP, Dir.LEFT) else setOf(Dir.DOWN, Dir.RIGHT)
                curDir == Dir.DOWN && nextSign == Sign.L -> if (Dir.LEFT in interiorDirectionSelector) setOf(Dir.DOWN, Dir.LEFT) else setOf(Dir.UP, Dir.RIGHT)
                curDir == Dir.LEFT && nextSign == Sign.L -> if (Dir.DOWN in interiorDirectionSelector) setOf(Dir.DOWN, Dir.LEFT) else setOf(Dir.UP, Dir.RIGHT)
                curDir == Dir.UP && nextSign == Sign.F -> if (Dir.RIGHT in interiorDirectionSelector) setOf(Dir.DOWN, Dir.RIGHT) else setOf(Dir.UP, Dir.LEFT)
                curDir == Dir.LEFT && nextSign == Sign.F -> if (Dir.DOWN in interiorDirectionSelector) setOf(Dir.DOWN, Dir.RIGHT) else setOf(Dir.UP, Dir.LEFT)
                else -> throw RuntimeException()
            }
            cur = next
            curDir = nextDir
            interiorDirectionSelector.forEach { dir ->
                possibleInterior1.add(
                    when (dir) {
                        Dir.UP -> cur.up()
                        Dir.DOWN -> cur.down()
                        Dir.LEFT -> cur.left()
                        Dir.RIGHT -> cur.right()
                    }
                )
                possibleInterior2.add(
                    when (dir.opposite()) {
                        Dir.UP -> cur.up()
                        Dir.DOWN -> cur.down()
                        Dir.LEFT -> cur.left()
                        Dir.RIGHT -> cur.right()
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
}

