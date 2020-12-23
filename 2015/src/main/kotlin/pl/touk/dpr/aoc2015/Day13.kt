package pl.touk.dpr.aoc2015

object Day13 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/13/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Int {
        val paths = readPaths(input)
        return generateRoutes(paths.keys.flatten().toSet(), paths).map { it.second + paths[setOf(it.first.first(), it.first.last())]!! }.max()!!
    }

    private fun readPaths(input: List<String>): MutableMap<Set<String>, Int> {
        val paths = mutableMapOf<Set<String>, Int>()
        input.forEach {
            val parts = it.split(Regex("[ .]+"))
            val key = setOf(parts[0], parts[10])
            val value = parts[3].toInt() * if (parts[2] == "gain") 1 else -1
            if (key in paths) {
                paths[key] = paths[key]!! + value
            } else {
                paths[key] = value
            }
        }
        return paths
    }

    private fun generateRoutes(toVisit: Set<String>, paths: Map<Set<String>, Int>): Set<Pair<List<String>, Int>> {
        if (toVisit.size == 1) {
            return setOf(Pair(listOf(toVisit.first()), 0))
        }
        return toVisit.flatMap { v ->
            val nestedRoutes = generateRoutes(toVisit - v, paths)
            val res = nestedRoutes.map { r ->
                Pair(listOf(v) + r.first, r.second + paths[setOf(v, r.first.first())]!!)
            }
            res
        }.toSet()
    }

    private fun part2(input: List<String>): Any {
        val paths = readPaths(input)
        paths.keys.flatten().toSet().forEach {
            paths[setOf("me", it)] = 0
        }
        return generateRoutes(paths.keys.flatten().toSet(), paths).map { it.second + paths[setOf(it.first.first(), it.first.last())]!! }.max()!!
    }

}