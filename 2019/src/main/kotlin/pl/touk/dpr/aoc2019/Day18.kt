package pl.touk.dpr.aoc2019

import java.util.PriorityQueue

object Day18 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/18/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
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

        // Closing blind tunnels
        var changes = 0
        while (true) {
            var changed = false
            for (j in input.indices) {
                for (i in input[j].indices) {
                    val point = Point(i, j)
                    if (map[point] == true && keysAndDoors[point] == null &&
                        point.neighbours().filter { map[it] != true }.size == 3
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
            val reachable = findReachable1(s.cur, map, s.toVisit)
//    println("Reachable " + reachable)
            reachable.forEach { e ->
                val key = e.key
//        println("Key $key")
                val moves = e.value
                val nextPos = keysAndDoors.filter { it.value == key }.keys.first()
                val newKeysAndDoors = s.toVisit.filter { it.value != key && it.value != key.toUpperCase() }
                val newState = State1(nextPos, newKeysAndDoors, s.length + moves, s.path + key)
//        println("Adding new state " + newState)
                pq.offer(newState)
            }
        }

        throw RuntimeException()
    }

    private fun part2(input: List<String>): Any {
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

        val curPoses = curPos.diag()
        curPos.neighbours().forEach {
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
                        point.neighbours().filter { map[it] != true }.size == 3 &&
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
                return s.localStates.sumBy { it.length }
            }
            s.localStates.forEach { ls ->
//        println("Checking local state $ls")
                val reachable = findReachable2(ls.cur, map, s.toVisit)
                reachable.forEach { e ->
                    val key = e.key
                    val moves = e.value
                    val nextPos = keysAndDoors.filter { it.value == key }.keys.first()
                    val newKeysAndDoors = s.toVisit.filter { it.value != key && it.value != key.toUpperCase() }
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

    data class Point(val x: Int, val y: Int) {
        fun neighbours(): List<Point> {
            return listOf(
                Point(x + 1, y),
                Point(x - 1, y),
                Point(x, y + 1),
                Point(x, y - 1),
            )
        }

        fun diag(): List<Point> {
            return listOf(
                Point(x + 1, y + 1),
                Point(x - 1, y + 1),
                Point(x + 1, y - 1),
                Point(x - 1, y - 1),
            )
        }
    }

    data class LocalState(val cur: Point, val length: Int) : Comparable<LocalState> {
        override fun compareTo(o: LocalState): Int {
            return length.compareTo(o.length)
        }
    }

    data class State1(
        val cur: Point,
        val toVisit: Map<Point, Char> = mapOf(),
        var length: Int = 0,
        val path: List<Char>
    ) : Comparable<State1> {
        fun ended() = toVisit.isEmpty()

        override fun compareTo(o: State1): Int {
            if (length == o.length) {
                return toVisit.size.compareTo(o.toVisit.size)
            }
            return length.compareTo(o.length)
        }
    }

    data class State2(
        val localStates: List<LocalState>, val toVisit: Map<Point, Char> = mapOf(),
        val path: List<Char>
    ) : Comparable<State2> {
        fun ended() = toVisit.isEmpty()

        override fun compareTo(o: State2): Int {
            val lengthCompare = localStates.sumBy { it.length }.compareTo(o.localStates.sumBy { it.length })
            if (lengthCompare == 0) {
                return toVisit.size.compareTo(o.toVisit.size)
            }
            return lengthCompare
        }
    }

    fun findReachable1(start: Point, map: Map<Point, Boolean>, keysAndDoors: Map<Point, Char>): Map<Char, Int> {
        val max = keysAndDoors.map { it.value.toLowerCase() }.toSet()
        val visited = mutableSetOf<Point>()
        val pq = PriorityQueue<LocalState>()
        pq.offer(LocalState(start, 0))
        val result = mutableMapOf<Char, Int>()
        while (!pq.isEmpty()) {
            if (result.size == max.size) {
                break
            }
            val localState = pq.poll()
            val cur = localState.cur
            visited.add(cur)
            cur.neighbours().filter { it in map && map[it] == true && it !in visited }.forEach {
                val newLocalState = LocalState(it, localState.length + 1)
                if (newLocalState.cur in keysAndDoors) {
                    val keyOrDoor = keysAndDoors[newLocalState.cur]!!
                    if (keyOrDoor.toUpperCase() != keyOrDoor) {
                        result[keyOrDoor] = newLocalState.length
                    }
                } else {
                    pq.offer(newLocalState)
                }
            }
        }
        return result
    }

    fun findReachable2(start: Point, map: Map<Point, Boolean>, keysAndDoors: Map<Point, Char>): Map<Char, Int> {
        val visited = mutableSetOf<Point>()
        val pq = PriorityQueue<LocalState>()
        pq.offer(LocalState(start, 0))
        val result = mutableMapOf<Char, Int>()
        while (!pq.isEmpty()) {
            val localState = pq.poll()
            val cur = localState.cur
            visited.add(cur)
            cur.neighbours().filter { it in map && map[it] == true && it !in visited }.forEach {
                val newLocalState = LocalState(it, localState.length + 1)
                if (newLocalState.cur in keysAndDoors) {
                    val keyOrDoor = keysAndDoors[newLocalState.cur]!!
                    if (keyOrDoor.toUpperCase() != keyOrDoor) {
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
