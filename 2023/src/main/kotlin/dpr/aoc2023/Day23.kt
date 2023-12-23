package dpr.aoc2023

import dpr.commons.Dir
import dpr.commons.Point2D
import dpr.commons.Util
import java.util.PriorityQueue
import java.util.Stack

object Day23 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/23/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/23/test1.txt")
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

    data class Route2(val point: Point2D, val seen: Set<Point2D>, val crossRoads: List<Point2D>) : Comparable<Route2> {
        val steps = seen.size - 1
        override fun compareTo(other: Route2): Int {
            val stepsDiff = steps - other.steps
            if(stepsDiff == 0){
                return crossRoads.size - other.crossRoads.size
            }
            return -stepsDiff
        }

    }

    private fun part2(lines: List<String>, knownPaths: Map<Set<Point2D>, Set<Point2D>>): Any {
        val (board, start, end) = readBoard(lines)
//        println("From $start to $end")
        val routes = PriorityQueue<Route2>()
        routes.offer(Route2(start, setOf(start), emptyList()))
        val crossRoads = board.filter { it.value != '#' }.keys - knownPaths.values.flatten().toSet()
        val finalPathBegin = (knownPaths.filter { end in it.key }.toList().single().first - end).single()
        val finalPath = knownPaths.filter { end in it.key }.toList().single().second
        val finalCrossRoad = finalPathBegin.neighboursCross().single { it in crossRoads }
        val knownEdges = knownPaths.map { (key, value) ->
            val newKey = key.map { it.neighboursCross().firstOrNull { it in crossRoads } ?: it }
            newKey to value + newKey
        }.toMap()
        var bestRoute = -1
//        val maxSize = crossRoads.size + knownPaths.values.flatten().size // 9412 is max
//        println("Max size is $maxSize")
//        println(crossRoads.size)
//        println(knownPaths.size)
//        val allCheckedPaths = knownPaths.values.flatten()
//        lines.indices.forEach { y ->
//            lines[y].indices.forEach { x ->
//                val point = Point2D(x, y)
//                if (point in crossRoads) {
//                    print('X')
//                } else if (point in allCheckedPaths) {
//                    print("#")
//                } else {
//                    print(' ')
////                    print(board[point] ?: '.')
//                }
//            }
//            println()
//        }
//        throw RuntimeException()
        val crossRoadsSize = crossRoads.size
        val mem = mutableSetOf<Pair<Point2D, List<Point2D>>>()
        while (routes.isNotEmpty()) {
            val cur = routes.poll()
            val memKey = cur.point to cur.crossRoads
            if (memKey in mem) {
                continue
            }
            mem.add(memKey)
//            println("Checking ${cur.point} and visited crossroads ${cur.crossRoads.size}/$crossRoadsSize, stack size ${routes.size}, finished lengths: $bestRoute")
            if (cur.point == end) {
                val length = cur.seen.size - 1  // minus start
                if (bestRoute < length) {
                    bestRoute = length
//                    println("Found better road - $bestRoute")
                    if(bestRoute == 6542) { // it's my solution checked by waiting too much
                        break
                    }
                }
//                println("Found path of length $length, best is $bestRoute")
                continue
            }
            val possibleNextEdges = knownEdges.filter { cur.point in it.key }
//            println(possibleNextEdges)
            possibleNextEdges.forEach { (vertices, points) ->
                val nextCrossRoad = (vertices - cur.point).single()
//                println("Next possible crossRoad = $nextCrossRoad")
                if (nextCrossRoad !in cur.crossRoads) {
                    if (nextCrossRoad == finalCrossRoad) {
                        //TODO end
                        routes.offer(Route2(end, cur.seen + points + finalPath, cur.crossRoads + nextCrossRoad))
//                    println("Reached final crossRoad - going on $finalPathBegin")
                    } else {
                        routes.offer(Route2(nextCrossRoad, cur.seen + points, cur.crossRoads + nextCrossRoad))
                    }
                }
            }
        }
        return bestRoute
        // 5603 is too low
        // 6091 is too low
        // 6331 is too low
        // 6343 is wrong
        // 6391 is wrong
        // 6414 is wrong
        // 6415 is wrong
        // 6474 is wrong
        // 6490 is wrong
    }
}

