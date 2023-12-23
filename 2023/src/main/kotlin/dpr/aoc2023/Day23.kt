package dpr.aoc2023

import dpr.commons.Dir
import dpr.commons.Point2D
import dpr.commons.Util
import java.util.Stack

object Day23 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
//        val lines = Util.getNotEmptyLinesFromFile("/23/input.txt")
        val lines = Util.getNotEmptyLinesFromFile("/23/test1.txt")
        val (result1, knownPaths) = part1(lines)
        println(result1)
        println(part2(lines, knownPaths))
    }

    data class Route(val point: Point2D, val dir: Dir, val length: Int)

    private fun part1(lines: List<String>): Pair<Int, Map<Set<Point2D>, Set<Point2D>>> {
        val (board, start, end) = readBoard(lines)
        val routes = Stack<Route>()
        routes.push(Route(start, Dir.S, 0))
        val finishedRoutes = mutableSetOf<Int>()
        val knownPaths = mutableMapOf<Set<Point2D>, Set<Point2D>>()
        while (routes.isNotEmpty()) {
            val cur = routes.pop()
            var counting = cur.point == start || board[cur.point] in setOf('<', '>', '^', 'v')
            var countStart = if (counting) cur.point else null
            var count = if (counting) mutableSetOf(cur.point) else mutableSetOf()

//            println("Checking $cur")
            var l = cur.length
            var point = cur.point
            var dir = cur.dir
            while (true) {
                val possibleNextSteps = listOf(dir, dir.turnLeft(), dir.turnRight()).mapNotNull { nextDir ->
                    val next = point.move(nextDir)
                    val tile = board[next]
                    when (tile) {
                        '#' -> null
                        '.' -> nextDir to next
                        '>' -> if (nextDir == Dir.E) nextDir to next else null
                        '<' -> if (nextDir == Dir.W) nextDir to next else null
                        '^' -> if (nextDir == Dir.N) nextDir to next else null
                        'v' -> if (nextDir == Dir.S) nextDir to next else null
                        else -> throw RuntimeException("Hitting $next")
                    }
                }
                l++
                if (possibleNextSteps.size > 1) {
                    if (counting) {
                        throw RuntimeException("You should not count here")
                    }
                    possibleNextSteps.forEach { (nextDir, next) ->
                        routes.push(Route(next, nextDir, l))
                    }
                    break
                } else {
                    val (nextDir, next) = possibleNextSteps.single()
                    if (counting) {
                        count.add(next)
                        if (board[next] in setOf('<', '>', '^', 'v') || next == end) {
//                            println("Adding path ${setOf(countStart!!, next)}")
                            knownPaths[setOf(countStart!!, next)] = count
                            counting = false
                        }
                    } else {
                        if (board[next] in setOf('<', '>', '^', 'v') || next == end) {
                            countStart = next
                            count = mutableSetOf(next)
                            counting = true
                        }
                    }
                    dir = nextDir
                    point = next
                    if (point == end) {
                        finishedRoutes.add(l)
                        break
                    }
                }
            }
        }
        return finishedRoutes.max() to knownPaths
    }

    private fun readBoard(lines: List<String>): Triple<MutableMap<Point2D, Char>, Point2D, Point2D> {
        val board = mutableMapOf<Point2D, Char>()
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                board[Point2D(x, y)] = c
            }
        }
        val start = Point2D(1, 0)
        val end = Point2D(lines[0].length - 2, lines.size - 1)
        return Triple(board, start, end)
    }

    data class Route2(val point: Point2D, val seen: Set<Point2D>, val crossRoads: List<Point2D>)

    private fun part2(lines: List<String>, knownPaths: Map<Set<Point2D>, Set<Point2D>>): Any {
        val (board, start, end) = readBoard(lines)
        val routes = Stack<Route2>()
        routes.push(Route2(start, setOf(start), emptyList()))
        val finishedRoutes = mutableSetOf<Int>()
        val crossRoads = board.filter { it.value != '#' }.keys - knownPaths.values.flatten().toSet()
//        println(crossRoads.size)
//        println(knownPaths.size)
//        val allCheckedPaths = knownPaths.values.flatten()
//        lines.indices.forEach { y ->
//            lines[y].indices.forEach { x ->
//                val point = Point2D(x, y)
//                if (point in crossRoads) {
//                    print('X')
//                } else if (point in allCheckedPaths) {
//                    print("O")
//                } else {
//                    print(board[point] ?: '.')
//                }
//            }
//            println()
//        }
//        throw RuntimeException()
        val mem = mutableSetOf<List<Point2D>>()
        while (routes.isNotEmpty()) {
            val cur = routes.pop()
            val memKey = cur.crossRoads
            if (memKey in mem) {
                continue
            }
            mem.add(memKey)
            println("Checking ${cur.point}, stack size ${routes.size}, finished lengths: ${finishedRoutes.maxByOrNull { it }}")
            val seen = cur.seen.toMutableSet()
            var point = cur.point
            val knownPath = knownPaths.filter { cur.point in it.key }
            if (knownPath.size > 1) {
                throw RuntimeException("$knownPath")
            } else if (knownPath.size == 1) {
//                println("Found known path $knownPath")
                val (edges, all) = knownPath.toList().single()
                point = (edges - cur.point).single()
                seen.addAll(all)
                if (point == end) {
                    finishedRoutes.add(seen.size)
                    continue
                }
            }
            while (true) {
                val possibleNextSteps = point.neighboursCross().filter { it !in seen && it in board && board[it] != '#' }
//                println(possibleNextSteps)
                if (possibleNextSteps.size > 1) { // crossroad
                    possibleNextSteps.filter { it !in seen }.forEach { next ->
                        routes.push(Route2(next, seen.toSet() + next))
                    }
                    break
                } else {
                    if (possibleNextSteps.isEmpty()) {
                        break
                    }
                    val next = possibleNextSteps.single()
                    if (next in seen) {
                        break
                    }

                    val knownPath = knownPaths.filter { next in it.key }
                    if (knownPath.size > 1) {
                        throw RuntimeException("$knownPath")
                    } else if (knownPath.size == 1) {
//                println("Found known path $knownPath")
                        val (edges, all) = knownPath.toList().single()
                        val secondEdge = (edges - cur.point).single()
                        if (secondEdge !in seen) {
                            point = secondEdge
                            seen.addAll(all)
                            if (point == end) {
                                finishedRoutes.add(seen.size)
                                continue
                            }
                        }
                    }

                    seen.add(next)
                    point = next
                    if (point == end) {
                        finishedRoutes.add(seen.size)
                        break
                    }
                }
            }
        }
        return finishedRoutes.max() - 1 // minus start
        // 5603 is too low
        // 6091 is too low
        // 6331 is too low
        // 6343 is wrong
    }
}

