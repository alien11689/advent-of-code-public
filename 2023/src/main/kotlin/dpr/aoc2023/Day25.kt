package dpr.aoc2023

import dpr.commons.Util

object Day25 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/25/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/25/test1.txt")
        println(part1(lines))
    }

    private fun part1(lines: List<String>): Any {
        var connections = readConnections(lines)
        val connectionsToCut = setOf(setOf("zhb", "vxr"), setOf("jbx", "sml"), setOf("szh", "vqj"))
        connections = connections - connectionsToCut
        val first = mutableSetOf(connectionsToCut.flatten().first()) // one of the cutting points
        while (true) {
            val (inside, outside) = connections.partition { it.any { it in first } }
            if (inside.isEmpty()) {
                break
            }
            first.addAll(inside.flatten())
            connections = outside.toSet()
        }
        return first.size * connections.flatten().toSet().size
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

