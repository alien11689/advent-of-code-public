package pl.touk.dpr.aoc2022

import java.util.PriorityQueue

object Day16 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/16/input.txt")
        println("Part 1:")
//        println(part1(Util.getNotEmptyLinesFromFile("/16/test1.txt")))
        // 1234 is wrong
        // 1691 is wrong
//        println(part1(lines))
        println("Part 2:")
        println(part2(Util.getNotEmptyLinesFromFile("/16/test1.txt")))
        println(part2(lines))
    }

    data class Room(val name: String, val rate: Int, val targets: List<String>)

    data class Mem(val room: String, val notOpenValves: Map<String, Int>)
    data class GlobalMem(val room: String, val notOpenValves: Map<String, Int>, val presure: Long)

    data class State(val room: String, val time: Int, val notOpenValves: Map<String, Int>, val presure: Long = 0, val memory: Set<Mem> = emptySet()) : Comparable<State> {
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
                options.add(this.copy(time = newTime, notOpenValves = newNotOpenValves, presure = presure + newTime * rate, memory = memory + Mem(room, newNotOpenValves)))
            }
            transitions[room]!!.forEach { newRoom ->
                val key = Mem(newRoom, notOpenValves)
                if (key !in memory) {
                    options.add(copy(room = newRoom, time = time - 1, memory = memory + key))
                }
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
        var globalMemory = mutableSetOf<GlobalMem>()
        while (pq.isNotEmpty()) {
            val cur = pq.poll()
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
        var maxPresure = 0L
        val pq = PriorityQueue<State2>()
        pq.offer(State2("AA", "AA", 26, valves))
        val globalMemory = mutableSetOf<GlobalMem2>()
        var generation = 0
        while (pq.isNotEmpty()) {
            val cur = pq.poll()
//            val globalKey = GlobalMem2(setOf(cur.room1, cur.room2), cur.notOpenValves, cur.presure, cur.time)
//            if (globalKey in globalMemory) {
//                continue
//            }
            if (++generation % 100000 == 0) {
                println("PQ size is ${pq.size} with time: ${cur.time}")
            }
            cur.nexts(transitions)
                .forEach {
                    if (it.presure > maxPresure) {
                        println("New leader ${it.presure}: ${it.room1},${it.room2} on time ${it.time} with notOpenValves ${it.notOpenValves}, pq size is ${pq.size}")
                        maxPresure = it.presure
                    }
                    if (it.notOpenValves.values.sumOf { r -> r * (it.time - 1) } + it.presure >= maxPresure) {
                        val globalKey = GlobalMem2(setOf(it.room1, it.room2), it.notOpenValves, it.presure, it.time)
                        if (globalKey !in globalMemory) {
                            globalMemory.add(globalKey)
                            pq.offer(it)
                        }
                    }
                }
        }
        return maxPresure
    }

    data class GlobalMem2(val room: Set<String>, val notOpenValves: Map<String, Int>, val presure: Long, val time: Int)

    data class Mem2(val rooms: Set<String>, val notOpenValves: Map<String, Int>)

    data class State2(val room1: String, val room2: String, val time: Int, val notOpenValves: Map<String, Int>, val presure: Long = 0, val memory: Set<Mem2> = emptySet()) :
        Comparable<State2> {
        override fun compareTo(other: State2): Int = if (other.time == time) {
            other.presure.compareTo(presure)
        } else other.time - time

        fun nexts(transitions: Map<String, List<String>>): List<State2> {
            if (time == 0 || notOpenValves.isEmpty()) {
                return emptyList()
            }
            val options = mutableListOf<State2>()
            if (room1 in notOpenValves && room2 in notOpenValves && room1 != room2) {
                val newTime = time - 1
                val rate1 = notOpenValves[room1]!!.toLong()
                val rate2 = notOpenValves[room2]!!.toLong()
                val newNotOpenValves = notOpenValves - room1 - room2
                val newPresure = presure + newTime * rate1 + newTime * rate2
                options.add(this.copy(time = newTime, notOpenValves = newNotOpenValves, presure = newPresure, memory = memory + Mem2(setOf(room1, room2), newNotOpenValves)))
            }
            if (room1 in notOpenValves) {
                val newTime = time - 1
                val rate1 = notOpenValves[room1]!!.toLong()
                val newNotOpenValves = notOpenValves - room1
                val newPresure = presure + newTime * rate1
                transitions[room2]!!.forEach { newRoom2 ->
                    options.add(
                        this.copy(
                            room2 = newRoom2,
                            time = newTime,
                            notOpenValves = newNotOpenValves,
                            presure = newPresure,
                            memory = memory + Mem2(setOf(room1, newRoom2), newNotOpenValves)
                        )
                    )
                }
            }
            if (room2 in notOpenValves) {
                val newTime = time - 1
                val rate2 = notOpenValves[room2]!!.toLong()
                val newNotOpenValves = notOpenValves - room2
                val newPresure = presure + newTime * rate2
                transitions[room1]!!.forEach { newRoom1 ->
                    options.add(
                        this.copy(
                            room1 = newRoom1,
                            time = newTime,
                            notOpenValves = newNotOpenValves,
                            presure = newPresure,
                            memory = memory + Mem2(setOf(newRoom1, room2), newNotOpenValves)
                        )
                    )
                }
            }
            transitions[room1]!!.forEach { newRoom1 ->
                transitions[room2]!!.forEach { newRoom2 ->
                    val key = Mem2(setOf(newRoom1, newRoom2), notOpenValves)
                    if (key !in memory) {
                        options.add(copy(room1 = newRoom1, room2 = newRoom2, time = time - 1, memory = memory + key))
                    }
                }
            }
            return options
        }

    }

}

