package dpr.aoc2015

object Day09 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/09/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Int {
        val paths = input.associate {
            val parts = it.split(Regex("[ =]+"))
            Pair(setOf(parts[0], parts[2]), parts[3].toInt())
        }
        return generateRoutes(paths.keys.flatten().toSet(), paths).minOf { it.second }
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
        val paths = input.associate {
            val parts = it.split(Regex("[ =]+"))
            Pair(setOf(parts[0], parts[2]), parts[3].toInt())
        }
        return generateRoutes(paths.keys.flatten().toSet(), paths).maxOf { it.second }
    }

}