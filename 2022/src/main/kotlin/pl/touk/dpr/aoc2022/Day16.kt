package pl.touk.dpr.aoc2022

import java.util.PriorityQueue

object Day16 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/16/input.txt")
        println("Part 1:")
        println(part1(Util.getNotEmptyLinesFromFile("/16/test1.txt")))
        println(part1(lines))
        println("Part 2:")
        println(part2(Util.getNotEmptyLinesFromFile("/16/test1.txt")))
        println(part2(lines))
        // 2122 is too low
        // 2328 is too low
        // 2346 is too low
    }

    data class Room(val name: String, val rate: Int, val targets: List<String>)

    data class State(val room: String, val time: Int, val notOpenValves: Map<String, Int>, val presure: Long = 0) : Comparable<State> {
        override fun compareTo(other: State): Int = time - other.time
        fun nexts(transitions: Map<String, List<String>>): List<State> {
            if (time == 0 || notOpenValves.isEmpty()) {
                return emptyList()
            }
            val options = mutableListOf<State>()
            if (room in notOpenValves) {
                val rate = notOpenValves[room]!!.toLong()
                val newTime = time - 1
                val newNotOpenValves = notOpenValves - room
                options.add(copy(time = newTime, notOpenValves = newNotOpenValves, presure = presure + newTime * rate))
            }
            transitions[room]!!.forEach { newRoom ->
                options.add(copy(room = newRoom, time = time - 1))
            }
            return options
        }

    }

    private fun part1(lines: List<String>): Any {
        val rooms = readRooms(lines)
        val valves = rooms.filter { it.rate > 0 }.associate { it.name to it.rate }
        val transitions = rooms.associate { it.name to it.targets }
        var maxPresure = 0L
        val pq = PriorityQueue<State>()
        pq.offer(State("AA", 30, valves))
        val theBest = mutableMapOf<Triple<String, Map<String, Int>, Long>, Int>()
        while (pq.isNotEmpty()) {
            val cur = pq.poll()
            val localKey = Triple(cur.room, cur.notOpenValves, cur.presure)
            val prev = theBest[localKey] ?: -1
            if (prev >= cur.time) {
                continue
            } else {
                theBest[localKey] = cur.time
            }
//            println("Checking ${cur.presure}: ${cur.room} on time ${cur.time} with notOpenValves ${cur.notOpenValves}")
            cur.nexts(transitions)
                .forEach {
                    if (it.presure > maxPresure) {
//                        println("New leader ${it.presure}: ${it.room} on time ${it.time} with notOpenValves ${it.notOpenValves}")
                        maxPresure = it.presure
                    }
                    if (it.notOpenValves.values.map { r -> r * (it.time - 1) }.sum() + it.presure >= maxPresure) {
                        pq.offer(it)
                    }
//                    val globalKey = GlobalMem(it.room, it.notOpenValves, it.presure)
//                    if (globalKey !in globalMemory) {
//                        globalMemory.add(globalKey)

//                    }
                }
        }
        return maxPresure
    }

    private fun readRooms(lines: List<String>) = lines.map { line ->
        val parts = line.split(";", ",", "=", " ")
        val name = parts[1]
        val rate = parts[5].toInt()
        val targets = parts.drop(11).take(100).filter { it.isNotBlank() }
        Room(name, rate, targets)
    }


    private fun part2(lines: List<String>): Any {
        val rooms = readRooms(lines)
        val valves = rooms.filter { it.rate > 0 }.associate { it.name to it.rate }
        val transitions = rooms.associate { it.name to it.targets }
        val startRoom = "AA"
        val realTransitions = findRealTransitions(startRoom, valves, transitions)
        return findMaxPresure(startRoom, valves, realTransitions)
    }

    private fun findMaxPresure(
        startRoom: String,
        valves: Map<String, Int>,
        realTransitions: MutableMap<Set<String>, Int>,
    ): Long {
        var maxPresure = 0L
        val pq = PriorityQueue<State3>()
        pq.offer(State3(Worker(listOf(startRoom), 26), Worker(listOf(startRoom), 26), valves))
        val mem = mutableSetOf<Set<Worker>>()
        var generation = 0
        while (pq.isNotEmpty()) {
            val cur = pq.poll()
            if (++generation % 100000 == 0) {
                println(
                    "     (Gen $generation) PQ size is ${pq.size}, max presure $maxPresure, mem size                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ${mem.size}"
                )
            }
            if (cur.presure > maxPresure) {
                println("New leader $cur")
                maxPresure = cur.presure
            }
            cur.nexts(realTransitions).forEach {
                val key = setOf(it.worker1, it.worker2)
                if (key !in mem) {
                    mem.add(key)
                    pq.offer(it)
                }
            }
        }
        return maxPresure
    }

    data class Worker(val path: List<String>, val time: Int) {
        fun pos(): String = path.last()
    }

    data class State3(val worker1: Worker, val worker2: Worker, val notOpenValves: Map<String, Int>, val presure: Long = 0) : Comparable<State3> {
        fun nexts(transitions: Map<Set<String>, Int>): Set<State3> {
            val options = mutableSetOf<State3>()
            notOpenValves.forEach {
                val target = it.key
                val rate = it.value
                val price1 = transitions[setOf(worker1.pos(), target)]!!
                val newWorker1 = worker1.copy(path = worker1.path + target, time = worker1.time - price1 - 1)
                if (newWorker1.time >= 0) {
                    options.add(copy(worker1 = newWorker1, notOpenValves = notOpenValves - target, presure = presure + newWorker1.time * rate))
                }
                val price2 = transitions[setOf(worker2.pos(), target)]!!
                val newWorker2 = worker2.copy(path = worker2.path + target, time = worker2.time - price2 - 1)
                if (newWorker2.time >= 0) {
                    options.add(copy(worker2 = newWorker2, notOpenValves = notOpenValves - target, presure = presure + newWorker2.time * rate))
                }
            }
            return options
        }

        override fun compareTo(other: State3): Int =
//            (other.worker1.time + other.worker2.time).compareTo(worker1.time + worker2.time)
            other.presure.compareTo(presure)
    }

    private fun findRealTransitions(
        startRoom: String,
        valves: Map<String, Int>,
        transitions: Map<String, List<String>>,
    ): MutableMap<Set<String>, Int> {
        val keys = (listOf(startRoom) + valves.keys).sorted()
        val realTransitions = mutableMapOf<Set<String>, Int>()
        keys.forEachIndexed { i, left ->
            val interesting = keys.drop(i + 1).toSet()
            //            println("$left is interesting about $interesting")
            val found = findShortestTransistionsToAll(left, interesting, transitions)
            //            println("$left -> $found")
            found.forEach {
                realTransitions[setOf(left, it.key)] = it.value
            }
        }
        return realTransitions
    }

    private fun findShortestTransistionsToAll(origin: String, interesting: Set<String>, transitions: Map<String, List<String>>): Map<String, Int> {
        val pq = PriorityQueue<List<String>> { o1, o2 ->
            o1.size.compareTo(o2.size)
        }
        pq.offer(listOf(origin))
        val search = interesting.toMutableSet()
        val found = mutableMapOf<String, Int>()
        while (pq.isNotEmpty()) {
            val cur = pq.poll()
            val last = cur.last()
            if (last in search) {
                found[last] = cur.size - 1
                search.remove(last)
            }
            transitions[last]!!.forEach { next ->
                if (next !in cur) {
                    pq.offer(cur + next)
                }
            }
        }
        return found
    }
}

