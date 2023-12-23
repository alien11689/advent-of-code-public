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

    data class Route2(val point: Point2D, val crossRoads: List<Point2D>, val steps: Int) : Comparable<Route2> {
        override fun compareTo(other: Route2): Int {
            val stepsDiff = steps - other.steps
            if (stepsDiff == 0) {
                return crossRoads.size - other.crossRoads.size
            }
            return -stepsDiff
        }

    }

    private fun part2(lines: List<String>, knownPaths: Map<Set<Point2D>, Set<Point2D>>): Any {
        val (board, start, end) = readBoard(lines)
//        println("From $start to $end")
        val routes = PriorityQueue<Route2>()
        routes.offer(Route2(start, emptyList(), 0))
        val crossRoads = board.filter { it.value != '#' }.keys - knownPaths.values.flatten().toSet()
        val finalPathBegin = (knownPaths.filter { end in it.key }.toList().single().first - end).single()
        val finalPath = knownPaths.filter { end in it.key }.toList().single().second.size
        val finalCrossRoad = finalPathBegin.neighboursCross().single { it in crossRoads }
        val knownEdges = knownPaths.map { (key, value) ->
            val newKey = key.map { it.neighboursCross().firstOrNull { next -> next in crossRoads } ?: it }.toSet()
            // edges are enriched on both sides but crossroads overlap so we add only on to the size (on start and end there is not need to enrich it)
            val newValue = if (end in newKey || start in newKey) value.size else value.size + 1
            newKey to newValue
        }.toMap()
        // little speed up that some longest paths need to be taken
        val mustHaveEdges = knownEdges.toList().sortedBy { -it.second }.take(knownEdges.size / 2).flatMap { it.first }.toSet() - end - start
//        println("strict graph {")
//        knownEdges.forEach {
//            val (a, b) = it.key.toList()
//            println("\"${a.x},${a.y}\" -- \"${b.x},${b.y}\" [label=\"${it.value.size}\"]")
//        }
//        println("}")
        var bestRoute = -1
        val mem = mutableSetOf<List<Point2D>>()
        val reachableMem = mutableMapOf<Set<Point2D>, Set<Point2D>>()
        while (routes.isNotEmpty()) {
            val cur = routes.poll()
            val memKey = cur.crossRoads
            if (memKey in mem) {
                continue
            }
            mem.add(memKey)
//            println("Checking ${cur.point} and visited crossroads ${cur.crossRoads.size}, stack size ${routes.size}, finished lengths: $bestRoute")
            if (cur.point == end) {
                val length = cur.steps
                if (bestRoute < length) {
                    bestRoute = length
//                    println(cur.crossRoads.joinToString(" -> ") { "${it.x},${it.y}" })
//                    println("Found better road - $bestRoute using ${cur.crossRoads.size}/${crossRoads.size} crossroads")
//                    if (bestRoute == 6542) { // it's my solution checked by waiting too much
//                        break
//                    }
                }
//                println("Found path of length $length using ${cur.crossRoads.size}/${crossRoads.size} crossroads, best is $bestRoute")
                continue
            }
            val unavailable = (cur.crossRoads - cur.point).toSet()
            val reachable = findReachable(finalCrossRoad, unavailable, knownEdges.keys, reachableMem)
            if (cur.point !in reachable) {
                continue
            }
            if (mustHaveEdges.intersect(reachable + cur.crossRoads) != mustHaveEdges) {
                continue
            }
            val possibleToAdd = knownEdges.filter { it.key.intersect(unavailable).isEmpty() }.values.sum()
            val potentialScore = cur.steps + possibleToAdd
            if (potentialScore < bestRoute) {
                continue
            }
            val possibleNextEdges = knownEdges.filter { cur.point in it.key }
//            println(possibleNextEdges)
            possibleNextEdges.forEach { (vertices, points) ->
                val nextCrossRoad = (vertices - cur.point).single()
//                println("Next possible crossRoad = $nextCrossRoad")
                if (nextCrossRoad !in cur.crossRoads && nextCrossRoad in reachable) {
                    if (nextCrossRoad == finalCrossRoad) {
                        routes.offer(Route2(end, cur.crossRoads + nextCrossRoad, cur.steps + points + finalPath))
//                    println("Reached final crossRoad - going on $finalPathBegin")
                    } else {
                        routes.offer(Route2(nextCrossRoad, cur.crossRoads + nextCrossRoad, cur.steps + points))
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

    private fun findReachable(
        finalCrossRoad: Point2D,
        unavailable: Set<Point2D>,
        knownEdges: Set<Set<Point2D>>,
        reachableMem: MutableMap<Set<Point2D>, Set<Point2D>>
    ): Set<Point2D> {
        if (unavailable in reachableMem) {
            return reachableMem[unavailable]!!
        }
        val possiblePaths = knownEdges.filter { it.intersect(unavailable).isEmpty() }.toSet()

        val reachable = mutableSetOf<Point2D>()

        val waitList = Stack<Point2D>()
        waitList.push(finalCrossRoad)
        while (waitList.isNotEmpty()) {
            val cur = waitList.pop()
            if (cur in reachable) {
                continue
            }
            reachable.add(cur)
            possiblePaths.filter { cur in it }.flatMap { it - cur }.filter { it !in reachable }.forEach { waitList.push(it) }
        }
        reachableMem[unavailable] = reachable
        return reachable
    }
}

