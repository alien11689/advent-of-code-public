package dpr.aoc2022

import dpr.commons.Util
import java.util.PriorityQueue

object Day16 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/16/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    data class Room(val name: String, val rate: Int, val targets: List<String>)

    private fun readRooms(lines: List<String>) = lines.map { line ->
        val parts = line.split(";", ",", "=", " ")
        val name = parts[1]
        val rate = parts[5].toInt()
        val targets = parts.drop(11).take(100).filter { it.isNotBlank() }
        Room(name, rate, targets)
    }

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

    @JvmStatic fun part1(lines: List<String>): Int {
        val (valves, transitions) = parseInput(lines)
        val startRoom = "AA"
        val realTransitions = findRealTransitions(startRoom, valves, transitions)
        val options = findPaths(startRoom, 30, valves, realTransitions)
        return options.values.max()
        //return findMaxPresure(startRoom, valves, realTransitions)
    }

    @JvmStatic fun part2(lines: List<String>): Int {
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

