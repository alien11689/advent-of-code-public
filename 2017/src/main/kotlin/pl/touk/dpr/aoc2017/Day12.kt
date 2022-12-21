package pl.touk.dpr.aoc2017

object Day12 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/12/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(routesDef: List<String>): Any {
        val routes = readRoutes(routesDef)

        val group = mutableSetOf(0)

        while (true) {
            val prev = group.toSet()
            group.addAll(routes.filter { it.any { it in group } }.flatten())
            if (prev == group.toSet()) {
                break
            }
        }
        return group.size
    }

    private fun readRoutes(routesDef: List<String>): MutableList<Set<Int>> {
        val routes = routesDef.flatMap {
            val parts = it.split(" ", limit = 3)
            val from = parts[0].toInt()
            val tos = parts[2].split(",").map { it.trim().toInt() }
            tos.map {
                setOf(from, it)
            }
        }.toMutableList()
        return routes
    }

    private fun part2(routesDef: List<String>): Any {
        val routes = readRoutes(routesDef)

        val used = mutableSetOf<Set<Int>>()

        var remain = routes.toSet() - used
        var groups = 0
        while (remain.isNotEmpty()) {
            val group = remain.first().toMutableSet()
            while (true) {
                val prev = group.toSet()
                val matchingRules = routes.filter { it !in used && it.any { it in group } }
                group.addAll(matchingRules.flatten())
                used.addAll(matchingRules)
                if (prev == group.toSet()) {
                    ++groups
                    remain = routes.toSet() - used
                    break
                }
            }
        }
        return groups
    }
}
