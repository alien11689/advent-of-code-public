package dpr.aoc2015

import dpr.commons.Util

object Day09 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/09/input.txt")
        val paths = readPaths(input)
        println(part1(paths))
        println(part2(paths))
    }

    @JvmStatic
    fun part1(paths: Map<Set<String>, Int>): Int {
        return generateRoutes(paths.keys.flatten().toSet(), paths).minOf { it.second }
    }

    private fun generateRoutes(toVisit: Set<String>, paths: Map<Set<String>, Int>): Set<Pair<List<String>, Int>> {
        if (toVisit.size == 1) {
            return setOf(Pair(listOf(toVisit.first()), 0))
        }
        return toVisit.flatMap { v ->
            val nestedRoutes = generateRoutes(toVisit - v, paths)
            nestedRoutes.map { r ->
                Pair(listOf(v) + r.first, r.second + paths[setOf(v, r.first.first())]!!)
            }
        }.toSet()
    }

    @JvmStatic
    fun part2(paths: Map<Set<String>, Int>): Int {
        return generateRoutes(paths.keys.flatten().toSet(), paths).maxOf { it.second }
    }

    @JvmStatic
    fun readPaths(input: List<String>) = input.associate {
        val parts = it.split(Regex("[ =]+"))
        Pair(setOf(parts[0], parts[2]), parts[3].toInt())
    }

}
