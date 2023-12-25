package dpr.aoc2023

import dpr.commons.Util
import java.util.PriorityQueue

object Day25 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/25/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/25/test1.txt")
        println(part1(lines))
    }

    private fun part1(lines: List<String>): Any {
        val connections = readConnections(lines)

        // naive solution creating all paths and counting most used vertices works well for my input but not for test input
        // when more than 6 most used vertices are taken (I tested with 10) than both input and test are fine
        // it needs ~2 minutes to finish
//        val takeMostUsedVertices = 6
//        val vertices = connections.flatten().toSet().sorted()
//        val counts = mutableMapOf<String, Int>()
//        vertices.forEach { counts[it] = 0 }
//        vertices.forEach { v ->
////            println("Checking $v")
//            findPaths(v, connections, counts)
//        }
//        val mostUsed = counts.toList().sortedBy { -it.second }.take(takeMostUsedVertices)
////        mostUsed.forEach { println(it) }
//        val mostUsedConnections = mostUsed.flatMap { v1 ->
//            mostUsed.map { v2 -> setOf(v1.first, v2.first) }
//        }.filter { it in connections }.toSet()
////        println("Most used connections $mostUsedConnections")
//        return splitGraph(connections - mostUsedConnections)

        // look at the graph in the resources
        val connectionsToCut = setOf(setOf("zhb", "vxr"), setOf("jbx", "sml"), setOf("szh", "vqj"))
        return splitGraph(connections - connectionsToCut)
    }

    private fun splitGraph(connections: Set<Set<String>>): Int {
        var left = connections
        val first = mutableSetOf(connections.flatten().first()) // one of the cutting points
        while (true) {
            val (inside, outside) = left.partition { it.any { it in first } }
            if (inside.isEmpty()) {
                break
            }
            first.addAll(inside.flatten())
            left = outside.toSet()
        }
        return first.size * left.flatten().toSet().size
    }

    private fun findPaths(v: String, connections: Set<Set<String>>, counts: MutableMap<String, Int>) {
        val visited = mutableSetOf<String>()
        val pq = PriorityQueue<List<String>>() { l1, l2 -> l1.size - l2.size }
        pq.offer(listOf(v))
        while (pq.isNotEmpty()) {
            val cur = pq.poll()
            val end = cur.last()
            if (end in visited) {
                continue
            }
            visited.add(end)
            (connections.filter { end in it }.flatten().toSet() - end).filter { it !in visited }.forEach {
                val newPath = cur + it
                pq.offer(newPath)
                if (newPath.size > 2) {
                    newPath.forEach {
                        counts[it] = counts[it]!! + 1
                    }
                }
            }
        }
    }

    private fun readConnections(lines: List<String>): Set<Set<String>> {
        val connections = mutableSetOf<Set<String>>()
        //        println("strict graph {")
        lines.forEach { line ->
            val (source, rest) = line.split(":").map { it.trim() }
            val targets = rest.split(" ").map { it.trim() }
            targets.forEach { target ->
                //                println("$source -- $target")
                connections.add(setOf(source, target))
            }
        }
        //        println("}")
//        println(connections.size)
//        println(connections.size)
        return connections
    }
}

