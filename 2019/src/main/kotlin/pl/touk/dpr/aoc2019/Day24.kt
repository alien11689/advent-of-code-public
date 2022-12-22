package pl.touk.dpr.aoc2019

import kotlin.math.pow

object Day24 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/24/input.txt")
        println(part1(input))
        println(part2(input))
    }

    data class Point(val x: Int, val y: Int, val level: Int = 0) {
        fun neighbours1(): Set<Point> {
            return setOf(
                    Point(x + 1, y),
                    Point(x - 1, y),
                    Point(x, y + 1),
                    Point(x, y - 1),
            )
        }

        fun neighbours2(): Set<Point> {
            val base = setOf(
                    Point(x + 1, y, level),
                    Point(x - 1, y, level),
                    Point(x, y + 1, level),
                    Point(x, y - 1, level),
            )
            return base.flatMap { p ->
                if (p.x < 0) {
                    listOf(Point(1, 2, level + 1))
                } else if (p.x > 4) {
                    listOf(Point(3, 2, level + 1))
                } else if (p.y < 0) {
                    listOf(Point(2, 1, level + 1))
                } else if (p.y > 4) {
                    listOf(Point(2, 3, level + 1))
                } else if (p.x == 2 && p.y == 2) {
                    if (x == 1) {
                        (0..4).map {
                            Point(0, it, level - 1)
                        }
                    } else if (x == 3) {
                        (0..4).map {
                            Point(4, it, level - 1)
                        }
                    } else if (y == 1) {
                        (0..4).map {
                            Point(it, 0, level - 1)
                        }
                    } else {
                        (0..4).map {
                            Point(it, 4, level - 1)
                        }
                    }
                } else {
                    listOf(p)
                }
            }.toSet()
        }
    }

    private fun createBoard(input: List<String>): Map<Point, Boolean> {
        val m = mutableMapOf<Point, Boolean>()
        for (i in input.indices) {
            for (j in input[i].indices) {
                m[Point(j, i)] = input[i][j] == '#'
            }
        }
        return m
    }

    private fun createBoard2(input: List<String>, level: Int): Map<Point, Boolean> {
        val m = mutableMapOf<Point, Boolean>()
        for (i in input.indices) {
            for (j in input[i].indices) {
                m[Point(j, i, level)] = input[i][j] == '#'
            }
        }
        m.remove(Point(2, 2, level))
        return m
    }

//    fun printBoard(m: Map<Point, Boolean>) {
//        for (i in 0 until 5) {
//            for (j in 0 until 5) {
//                print(if (m[Point(j, i)] == true) '#' else '.')
//            }
//            println()
//
//        }
//    }

    private fun tick(cur: Map<Point, Boolean>): Map<Point, Boolean> {
        return cur.map { e ->
            val p = e.key
            val exists = e.value
            val livingNeighbours = p.neighbours1().count { it in cur && cur[it]!! }
            if (exists) {
                p to (livingNeighbours == 1)
            } else {
                p to (livingNeighbours in setOf(1, 2))
            }
        }.toMap()
    }

    private fun tick2(cur: Map<Point, Boolean>): Map<Point, Boolean> {
        val points = (cur.filter { it.value }.keys.flatMap { it.neighbours2() } + cur.filter { it.value }.keys)
        return points.associate { p ->
            val exists = cur[p] ?: false
            val livingNeighbours = p.neighbours2().count { it in cur && cur[it] == true }
            if (exists) {
                p to (livingNeighbours == 1)
            } else {
                p to (livingNeighbours in setOf(1, 2))
            }
        }
    }

    private fun part1(input: List<String>): Any {
        val memory = mutableSetOf<Map<Point, Boolean>>()
        var board = createBoard(input)
        while (true) {
//            printBoard(board)
            if (board in memory) {
                break
            }
            memory.add(board)
            board = tick(board)
//            println("========================")
        }

//        printBoard(board)
        val res = board.filter { it.value }.map {
            val p = it.key
            val power = p.y * 5 + p.x
            (2.0).pow(power.toDouble()).toLong()
        }.sum()
        return res
    }

    private fun part2(input: List<String>): Any {
        val minutes = 200
        var board = createBoard2(input, 0)

        var iter = 0
        while (iter < minutes) {
            board = tick2(board)
            ++iter
        }
        return board.count { it.value }
    }
}
