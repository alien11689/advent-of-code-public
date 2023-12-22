package dpr.aoc2019

import dpr.commons.Util
import java.util.PriorityQueue
import dpr.commons.Point2D as Point

object Day18 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/18/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val (map, keysAndDoors, curPos) = parseInput(input)

        // Closing blind tunnels
        var changes = 0
        while (true) {
            var changed = false
            for (j in input.indices) {
                for (i in input[j].indices) {
                    val point = Point(i, j)
                    if (map[point] == true && keysAndDoors[point] == null &&
                        point.neighboursCross().filter { map[it] != true }.size == 3
                    ) {
                        changed = true
                        ++changes
                        map[point] = false
                    }
                }
            }
            if (!changed) {
                break
            }
        }
//        println("Changes: $changes")

        val state = State1(curPos, keysAndDoors, 0, listOf())

        val pq = PriorityQueue<State1>()
        pq.offer(state)

        val mem = mutableSetOf<Pair<Point, Map<Point, Char>>>()

        while (!pq.isEmpty()) {
            val s = pq.poll()
//    println("=========================")
            if (s.cur to s.toVisit in mem) {
                continue
            }
            mem.add(s.cur to s.toVisit)
//    println("Checking $s")
            if (s.ended()) {
                return s.length
            }
//            println(s.length)
            val passages: Set<Char> = keysAndDoors.map { it.value.lowercaseChar() }.toSet()
            val reachable = findReachable(s.cur, map, s.toVisit, passages)
//    println("Reachable " + reachable)
            reachable.forEach { (key, moves) ->
                val nextPos = keysAndDoors.filter { it.value == key }.keys.first()
                val newKeysAndDoors = s.toVisit.filter { it.value != key && it.value != key.uppercaseChar() }
                val newState = State1(nextPos, newKeysAndDoors, s.length + moves, s.path + key)
//        println("Adding new state " + newState)
                pq.offer(newState)
            }
        }

        throw RuntimeException()
    }

    private fun parseInput(input: List<String>): Triple<MutableMap<Point, Boolean>, MutableMap<Point, Char>, Point> {
        val map = mutableMapOf<Point, Boolean>()
        val keysAndDoors = mutableMapOf<Point, Char>()
        var curPosSearch: Point? = null
        for (j in input.indices) {
            for (i in input[j].indices) {
                val field = input[j][i]
                val point = Point(i, j)
                when (field) {
                    '#' -> map[point] = false
                    '.' -> map[point] = true
                    '@' -> {
                        map[point] = true
                        curPosSearch = point
                    }

                    else -> {
                        map[point] = true
                        keysAndDoors[point] = field
                    }
                }
            }
        }
        val curPos = curPosSearch!!
        return Triple(map, keysAndDoors, curPos)
    }

    private fun part2(input: List<String>): Any {
        val (map, keysAndDoors, curPos) = parseInput(input)

        val curPoses = curPos.neighboursDiag()
        curPos.neighboursCross().forEach {
            map[it] = false
        }

        // Closing blind tunnels
        var changes = 0
        while (true) {
            var changed = false
            for (j in input.indices) {
                for (i in input[j].indices) {
                    val point = Point(i, j)
                    if (map[point] == true && keysAndDoors[point] == null &&
                        point.neighboursCross().filter { map[it] != true }.size == 3 &&
                        point !in curPoses
                    ) {
                        changed = true
                        ++changes
                        map[point] = false
                    }
                }
            }
            if (!changed) {
                break
            }
        }
//        println("Changes: $changes")

        val state = State2(curPoses.map { LocalState(it, 0) }, keysAndDoors, listOf())

        val pq = PriorityQueue<State2>()
        pq.offer(state)

        val mem = mutableSetOf<Map<Point, Char>>()

        while (!pq.isEmpty()) {
            val s = pq.poll()
//    println("=========================")
            if (s.toVisit in mem) {
                continue
            }
            mem.add(s.toVisit)
//    println("Checking $s")
//    println(s.localStates.sum { it.length })
            if (s.ended()) {
                return s.localStates.sumOf { it.length }
            }
            s.localStates.forEach { ls ->
//        println("Checking local state $ls")
                val reachable = findReachable(ls.cur, map, s.toVisit, null)
                reachable.forEach { (key, moves) ->
                    val nextPos = keysAndDoors.filter { it.value == key }.keys.first()
                    val newKeysAndDoors = s.toVisit.filter { it.value != key && it.value != key.uppercaseChar() }
                    val newLocalState = LocalState(nextPos, ls.length + moves)
//            println("current local states " + s.localStates)
                    val newLocalStates = s.localStates.filter { local -> local != ls } + newLocalState
//            println("new local states " + newLocalStates)
                    val newState = State2(newLocalStates, newKeysAndDoors, s.path + key)
                    pq.offer(newState)
                }
            }
        }

        throw RuntimeException()
    }

    data class LocalState(val cur: Point, val length: Int) : Comparable<LocalState> {
        override fun compareTo(other: LocalState): Int {
            return length.compareTo(other.length)
        }
    }

    data class State1(
        val cur: Point,
        val toVisit: Map<Point, Char> = mapOf(),
        var length: Int = 0,
        val path: List<Char>
    ) : Comparable<State1> {
        fun ended() = toVisit.isEmpty()

        override fun compareTo(other: State1): Int {
            if (length == other.length) {
                return toVisit.size.compareTo(other.toVisit.size)
            }
            return length.compareTo(other.length)
        }
    }

    data class State2(
        val localStates: List<LocalState>, val toVisit: Map<Point, Char> = mapOf(),
        val path: List<Char>
    ) : Comparable<State2> {
        fun ended() = toVisit.isEmpty()

        override fun compareTo(other: State2): Int {
            val lengthCompare = localStates.sumOf { it.length }.compareTo(other.localStates.sumOf { it.length })
            if (lengthCompare == 0) {
                return toVisit.size.compareTo(other.toVisit.size)
            }
            return lengthCompare
        }
    }

    private fun findReachable(start: Point, map: Map<Point, Boolean>, keysAndDoors: Map<Point, Char>, passages: Set<Char>?): Map<Char, Int> {
        val visited = mutableSetOf<Point>()
        val pq = PriorityQueue<LocalState>()
        pq.offer(LocalState(start, 0))
        val result = mutableMapOf<Char, Int>()
        while (!pq.isEmpty()) {
            if (passages != null && result.size == passages.size) {
                break
            }
            val localState = pq.poll()
            val cur = localState.cur
            visited.add(cur)
            cur.neighboursCross().filter { it in map && map[it] == true && it !in visited }.forEach {
                val newLocalState = LocalState(it, localState.length + 1)
                if (newLocalState.cur in keysAndDoors) {
                    val keyOrDoor = keysAndDoors[newLocalState.cur]!!
                    if (keyOrDoor.uppercaseChar() != keyOrDoor) {
                        result[keyOrDoor] = newLocalState.length
                    }
                } else {
                    pq.offer(newLocalState)
                }
            }
        }
        return result
    }
}
