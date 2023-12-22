package dpr.aoc2019

import dpr.commons.Util
import java.util.PriorityQueue
import dpr.commons.Point2D as Point

object Day20 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/20/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val (map, warps) = parseInput(input)

        val betterWarps = warps.map { warp ->
            val k = warp.key.flatMap { it.neighboursCross() }.find { map[it] == true }
            k to warp.value
        }.toMap()

        val warpPoints = betterWarps.keys

        val start = betterWarps.filter { it.value == setOf('A') }.toList().first().first!!
        val dest = betterWarps.filter { it.value == setOf('Z') }.toList().first().first!!

        val visited = mutableSetOf<Visited>()

        val pq = PriorityQueue<State1>()
        pq.offer(State1(start, 0))

        while (true) {
//    println("pq size ${pq.size()}")
            val state = pq.poll()
//    println("Checking $state")
            if (state.cur == dest) {
//                println("State is ${state}")
//                println("Get in ${state.length} steps")
                return state.length
            }
            if (Visited(state.cur) in visited) {
                continue
            }
            visited.add(Visited(state.cur))
            state.cur.neighboursCross().filter { map[it] == true }.forEach { checkedPoint ->
                if (checkedPoint in warpPoints && checkedPoint !in setOf(dest, start)) {
                    val warp = betterWarps[checkedPoint]!!
//            println("Using warp $warp")
                    val to = betterWarps.filter { it.value == warp && it.key != checkedPoint }.toList().first().first!!
                    pq.offer(State1(to, state.length + 2, state.path + listOf(warp)))
                } else {
                    pq.offer(State1(checkedPoint, state.length + 1, state.path))
                }
            }

        }

    }

    private fun parseInput(input: List<String>): Pair<MutableMap<Point, Boolean>, MutableMap<Set<Point>, Set<Char>>> {
        val map = mutableMapOf<Point, Boolean>()

        val warps = mutableMapOf<Set<Point>, Set<Char>>()

        val sizeY = input.size

        for (j in 0 until sizeY) {
            for (i in input[j].indices) {
                val p = Point(i, j)
                when (input[j][i]) {
                    '.' -> map[p] = true
                    '#' -> map[p] = false
                    ' ' -> {
                    }

                    else -> {
                        val m = (p.neighboursCross() + p).filter {
                            it.x >= 0 && it.x < input[j].length && it.y >= 0 && it.y < sizeY
                        }.map { it to input[it.y][it.x] }.filter { it.second !in setOf(' ', '#', '.') }.toMap()
                        warps[m.keys] = m.values.toSet()
                    }
                }
            }
        }
        return Pair(map, warps)
    }

    private fun part2(input: List<String>): Any {
        val (map, warps) = parseInput(input)

        val sizeY = input.size
        val sizeX = input[2].length

        val betterWarps = warps.map { warp ->
            val k = warp.key.flatMap { it.neighboursCross() }.find { map[it] == true }
            k!! to warp.value
        }.toMap()

        val warpPoints = betterWarps.keys

        val start = betterWarps.filter { it.value == setOf('A') }.toList().first().first
        val dest = betterWarps.filter { it.value == setOf('Z') }.toList().first().first

        val pq = PriorityQueue<State2>()
        pq.offer(State2(start, 0))

        val outerWarps = betterWarps.keys.filter { it.x in listOf(2, sizeX - 3) || it.y in listOf(2, sizeY - 3) }

        val visited = mutableSetOf<Visited>()

        while (true) {
//    println("pq size ${pq.size()}")
            val state = pq.poll()
//    println("Checking $state")
//    println("Length ${state.length}")
            if (state.cur == dest && state.level == 0) {
//                println("State $state")
//                println("Get in ${state.length} steps")
                return state.length
            }

            if (Visited(state.cur, state.level) in visited) {
                continue
            }
            visited.add(Visited(state.cur, state.level))

            state.cur.neighboursCross().filter { map[it] == true }.forEach { checkedPoint ->
                if (checkedPoint in setOf(dest, start) && state.level != 0) {
                    // dest and start are walls on inner levels
                } else if (checkedPoint in warpPoints &&
                    state.level == 0 &&
                    checkedPoint !in setOf(dest, start) &&
                    checkedPoint in outerWarps
                ) {
                    // on level 0 outers do not work
                } else {
                    if (checkedPoint in warpPoints && checkedPoint !in setOf(dest, start)) {
                        val warp = betterWarps[checkedPoint]
//                println("Using warp $warp")
                        val to =
                            betterWarps.filter { it.value == warp && it.key != checkedPoint }.toList().first().first
                        val level = if (checkedPoint in outerWarps) {
                            state.level - 1
                        } else {
                            state.level + 1
                        }
                        val element = PathElement(warp!!, checkedPoint, to, level, state.length + 2)
                        if (state.path.isNotEmpty() && state.path.last().warp == warp) {
                            // do nothing - cycle
                        } else {
                            pq.offer(State2(to, state.length + 2, state.path + element, level))
                        }
                    } else {
                        pq.offer(State2(checkedPoint, state.length + 1, state.path, state.level))
                    }
                }
            }

        }
    }

    data class PathElement(val warp: Set<Char>, val from: Point, val to: Point, val level: Int, val length: Int)

    data class State1(val cur: Point, val length: Int, val path: List<Set<Char>> = listOf()) : Comparable<State1> {
        override fun compareTo(other: State1): Int {
            return length.compareTo(other.length)
        }
    }

    data class State2(val cur: Point, val length: Int, val path: List<PathElement> = listOf(), val level: Int = 0) :
        Comparable<State2> {
        override fun compareTo(other: State2): Int {
            return length.compareTo(other.length)
        }
    }

    data class Visited(val p: Point, val level: Int = 0)
}
