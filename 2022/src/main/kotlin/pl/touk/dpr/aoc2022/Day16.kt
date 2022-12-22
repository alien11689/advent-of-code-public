package pl.touk.dpr.aoc2022

import java.util.PriorityQueue

object Day16 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/16/input.txt")
//        println("Part 1:")
//        println(part1FirstApproach(Util.getNotEmptyLinesFromFile("/16/test1.txt")))
//        println(part1FirstApproach(lines))
        println(part1(lines))
//        println("Part 2:")
//        println(part2FirstApproach(Util.getNotEmptyLinesFromFile("/16/test1.txt")))
//        println(part2FirstApproach(lines))
//        println(part2(Util.getNotEmptyLinesFromFile("/16/test1.txt")))
        println(part2(lines))
        // 2122 is too low
        // 2328 is too low
        // 2346 is too low
        // 2422 is correct
    }

    data class Room(val name: String, val rate: Int, val targets: List<String>)

//    data class State1(val room: String, val time: Int, val notOpenValves: Map<String, Int>, val presure: Long = 0) : Comparable<State1> {
//        override fun compareTo(other: State1): Int = time - other.time
//        fun nexts(transitions: Map<Set<String>, Int>): Set<State1> {
//            val options = mutableSetOf<State1>()
//            notOpenValves.forEach {
//                val target = it.key
//                val rate = it.value
//                val price1 = transitions[setOf(room, target)]!!
//                val newTime = time - price1
//                if (newTime >= 0) {
//                    options.add(copy(room = target, notOpenValves = notOpenValves - target, presure = presure + newTime * rate, time = newTime))
//                }
//            }
//            return options
//        }
//
//    }

//    private fun part1FirstApproach(lines: List<String>): Any {
//        val rooms = readRooms(lines)
//        val valves = rooms.filter { it.rate > 0 }.associate { it.name to it.rate }
//        val transitions = rooms.associate { it.name to it.targets }
//        val realTransitions = findRealTransitions("AA", valves, transitions)
//        var maxPresure = 0L
//        val pq = PriorityQueue<State1>()
//        pq.offer(State1("AA", 30, valves))
//        val theBest = mutableMapOf<Triple<String, Map<String, Int>, Long>, Int>()
//        while (pq.isNotEmpty()) {
//            val cur = pq.poll()
//            val localKey = Triple(cur.room, cur.notOpenValves, cur.presure)
//            val prev = theBest[localKey] ?: -1
//            if (prev >= cur.time) {
//                continue
//            } else {
//                theBest[localKey] = cur.time
//            }
////            println("Checking ${cur.presure}: ${cur.room} on time ${cur.time} with notOpenValves ${cur.notOpenValves}")
//            cur.nexts(realTransitions)
//                .forEach {
//                    if (it.presure > maxPresure) {
//                        maxPresure = it.presure
//                    }
//                    if (it.notOpenValves.values.sumOf { r -> r * it.time } + it.presure > maxPresure) {
//                        pq.offer(it)
//                    }
//                }
//        }
//        return maxPresure
//    }

    private fun readRooms(lines: List<String>) = lines.map { line ->
        val parts = line.split(";", ",", "=", " ")
        val name = parts[1]
        val rate = parts[5].toInt()
        val targets = parts.drop(11).take(100).filter { it.isNotBlank() }
        Room(name, rate, targets)
    }


//    private fun part2FirstApproach(lines: List<String>): Any {
//        val rooms = readRooms(lines)
//        val valves = rooms.filter { it.rate > 0 }.associate { it.name to it.rate }
//        val transitions = rooms.associate { it.name to it.targets }
//        val startRoom = "AA"
//        val realTransitions = findRealTransitions(startRoom, valves, transitions)
//        return findMaxPresure(startRoom, valves, realTransitions)
//    }

//    private fun findMaxPresure(
//        startRoom: String,
//        valves: Map<String, Int>,
//        realTransitions: Map<Set<String>, Int>,
//    ): Long {
//        var maxPresure = 0L
//        val pq = PriorityQueue<State2>()
//        pq.offer(State2(Worker(listOf(startRoom), 26), Worker(listOf(startRoom), 26), valves))
//        val mem = mutableSetOf<Set<Worker>>()
////        var generation = 0
//        while (pq.isNotEmpty()) {
//            val cur = pq.poll()
////            if (++generation % 100000 == 0) {
////                println("     (Gen $generation) PQ size is ${pq.size}, max presure $maxPresure, mem size   ${mem.size}")
////            }
//            cur.nexts(realTransitions).forEach {
//                val key = setOf(it.worker1, it.worker2)
//                if (key !in mem) {
//                    mem.add(key)
//                    if (it.presure > maxPresure) {
////                        println("New leader $it")
//                        maxPresure = it.presure
//                    }
//                    // function below could be max or min -> for max gets OOM error, for min works and gives good answer...
//                    if (it.notOpenValves.values.sumOf { r -> r * min(it.worker1.time, it.worker2.time) } + it.presure > maxPresure) {
//                        pq.offer(it)
//                    }
//                }
//            }
//        }
//        return maxPresure
//    }

//    data class Worker(val path: List<String>, val time: Int) {
//        fun pos(): String = path.last()
//    }

//    data class State2(val worker1: Worker, val worker2: Worker, val notOpenValves: Map<String, Int>, val presure: Long = 0) : Comparable<State2> {
//        fun nexts(transitions: Map<Set<String>, Int>): Set<State2> {
//            val options = mutableSetOf<State2>()
//            notOpenValves.forEach {
//                val target = it.key
//                val rate = it.value
//                val price1 = transitions[setOf(worker1.pos(), target)]!!
//                val newWorker1 = worker1.copy(path = worker1.path + target, time = worker1.time - price1)
//                if (newWorker1.time >= 0) {
//                    options.add(copy(worker1 = newWorker1, notOpenValves = notOpenValves - target, presure = presure + newWorker1.time * rate))
//                }
//                val price2 = transitions[setOf(worker2.pos(), target)]!!
//                val newWorker2 = worker2.copy(path = worker2.path + target, time = worker2.time - price2)
//                if (newWorker2.time >= 0) {
//                    options.add(copy(worker2 = newWorker2, notOpenValves = notOpenValves - target, presure = presure + newWorker2.time * rate))
//                }
//            }
//            return options
//        }
//
//        override fun compareTo(other: State2): Int =
////            (other.worker1.time + other.worker2.time).compareTo(worker1.time + worker2.time)
//            other.presure.compareTo(presure)
//    }

    private fun findRealTransitions(
            startRoom: String,
            valves: Map<String, Int>,
            transitions: Map<String, List<String>>,
    ): Map<Set<String>, Int> {
        val keys = (listOf(startRoom) + valves.keys).sorted()
        return keys.flatMapIndexed { i, left ->
            val interesting = keys.drop(i + 1).toSet()
            //            println("$left is interesting about $interesting")
            val found = findShortestTransitionsToAll(left, interesting, transitions)
            //            println("$left -> $found")
            found.map {
                setOf(left, it.key) to it.value
            }
        }.toMap()
    }

    private fun findShortestTransitionsToAll(origin: String, interesting: Set<String>, transitions: Map<String, List<String>>): Map<String, Int> {
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
                found[last] = cur.size
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

    private fun part1(lines: List<String>): Any {
        val (valves, transitions) = parseInput(lines)
        val startRoom = "AA"
        val realTransitions = findRealTransitions(startRoom, valves, transitions)
        val options = findPaths(startRoom, 30, valves, realTransitions)
        return options.values.max()
        //return findMaxPresure(startRoom, valves, realTransitions)
    }

    private fun part2(lines: List<String>): Any {
        val (valves, transitions) = parseInput(lines)
        val startRoom = "AA"
        val realTransitions = findRealTransitions(startRoom, valves, transitions)
        val options = findPaths(startRoom, 26, valves, realTransitions)
        val interestingPaths = mutableMapOf<Set<String>, Int>()
        options.forEach {
            val k = it.key.toSet() - "AA"
            if (k !in interestingPaths || k in interestingPaths && interestingPaths[k]!! < it.value) {
                interestingPaths[k] = it.value
            }
        }
        val results = interestingPaths.toList().sortedByDescending { it.second }
        var maxPresureSum = 0
        for (i in results.indices) {
            val cur = results[i]
            if (cur.second * 2 < maxPresureSum) {
                break
            }
            val opposite = results.drop(i + 1).find { it.first.intersect(cur.first).isEmpty() }
            if (opposite != null) {
                val presure = cur.second + opposite.second
                if (presure > maxPresureSum) {
                    maxPresureSum = presure
//                    println("Found new leader $maxPresureSum")
                }
            }
        }
        return maxPresureSum
    }

    private fun parseInput(lines: List<String>): Pair<Map<String, Int>, Map<String, List<String>>> {
        val rooms = readRooms(lines)
        val valves = rooms.filter { it.rate > 0 }.associate { it.name to it.rate }
        val transitions = rooms.associate { it.name to it.targets }
        return Pair(valves, transitions)
    }

    private fun findPaths(startRoom: String, time: Int, valves: Map<String, Int>, realTransitions: Map<Set<String>, Int>): Map<List<String>, Int> {
        val options = mutableMapOf<List<String>, Int>()
        val pq = PriorityQueue<State>()
        pq.offer(State(listOf(startRoom), time, valves))
        while (pq.isNotEmpty()) {
            val cur = pq.poll()
            cur.nexts(realTransitions).forEach {
                options[it.path] = it.presure
                pq.offer(it)
            }
        }
        return options
    }

    data class State(val path: List<String>, val time: Int, val notOpenValves: Map<String, Int>, val presure: Int = 0) : Comparable<State> {
        fun nexts(transitions: Map<Set<String>, Int>): List<State> {
            val options = mutableListOf<State>()
            notOpenValves.forEach {
                val target = it.key
                val rate = it.value
                val price = transitions[setOf(path.last(), target)]!!
                if (price < time) {
                    val newTime = time - price
                    options.add(copy(path = path + target, time = newTime, presure = presure + newTime * rate, notOpenValves = notOpenValves - target))
                }
            }
            return options
        }

        override fun compareTo(other: State): Int = other.presure - presure
    }

}

