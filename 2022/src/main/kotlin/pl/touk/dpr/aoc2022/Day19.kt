package pl.touk.dpr.aoc2022

import java.util.PriorityQueue

object Day19 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/19/input.txt")
        println("Part 1:")
        println(part1(Util.getNotEmptyLinesFromFile("/19/test1.txt")))
//        println(part1(lines))
//        println("Part 2:")
//        println(part2(Util.getNotEmptyLinesFromFile("/19/test1.txt")))
//        println(part2(lines))
    }

    data class State(val time: Int, val materials: Map<Material, Int>, val robots: Map<Material, Int>) : Comparable<State> {
        val geodes = robots[Material.OBSIDIAN] ?: 0
        val obsidians = robots[Material.OBSIDIAN] ?: 0
        val clays = robots[Material.OBSIDIAN] ?: 0
        val ores = robots[Material.OBSIDIAN] ?: 0
        override fun compareTo(other: State): Int =
            if (geodes != other.geodes) {
                other.geodes.compareTo(geodes)
            } else if (obsidians != other.obsidians) {
                other.obsidians.compareTo(obsidians)
            } else if (clays != other.clays) {
                other.clays.compareTo(clays)
            } else if (ores != other.ores) {
                other.ores.compareTo(ores)
            } else other.time.compareTo(time)

        fun nexts(robotCosts: Map<Material, Map<Material, Int>>): List<State> {
            val options = mutableListOf<State>()
            if (Material.values().all { (robots[it] ?: 0) > 0 }) {
                options.add(copy(time = 0, materials = merge(materials, times(robots, time))))
            }
            robotCosts.forEach { e ->
                val factory = e.key
                val cost = e.value
                if (robotCosts.keys.containsAll(cost.keys)) {
                    var curMaterials = materials
                    var nextTime = time - 1
                    while (nextTime >= 1) {
//                    println(" Checking if I can afford $e with $curMaterials on $nextTime")
                        if (cost.all { (curMaterials[it.key] ?: 0) >= it.value }) {
                            options.add(copy(time = nextTime, materials = merge(minus(curMaterials, cost), robots), robots = merge(robots, mapOf(factory to 1))))
                            break
                        } else {
                            nextTime--
                            curMaterials = merge(curMaterials, robots)
                        }
                    }
                }
            }
            return options
        }
    }

    private fun merge(first: Map<Material, Int>, second: Map<Material, Int>): Map<Material, Int> =
        Material.values().associateWith { (first[it] ?: 0) + (second[it] ?: 0) }

    private fun minus(first: Map<Material, Int>, second: Map<Material, Int>): Map<Material, Int> =
        Material.values().associateWith { (first[it] ?: 0) - (second[it] ?: 0) }

    private fun times(first: Map<Material, Int>, repeat: Int): Map<Material, Int> =
        Material.values().associateWith { (first[it] ?: 0) * repeat }

    data class Blueprint(val id: Int, val robotCosts: Map<Material, Map<Material, Int>>) {
        fun findMostGeode(turns: Int): Long {
            val memory = mutableSetOf<State>()
            var geodeMax = 0
            val pq = PriorityQueue<State>()
            pq.add(State(turns, emptyMap(), mapOf(Material.ORE to 1)))
            while (pq.isNotEmpty()) {
                val cur = pq.poll()
//                println("Anylizing $cur, pq size: ${pq.size}, max geode: $geodeMax")
                cur.nexts(robotCosts).forEach {
                    if (it.time == 0) {
                        val geodeCount = it.materials[Material.GEODE] ?: 0
                        if (geodeMax < geodeCount) {
                            geodeMax = geodeCount
                            println("New Max geode $geodeMax")
                        }
                    } else {
                        if (it !in memory) {
//                            println("Added state $it")
                            memory.add(it)
                            pq.offer(it)
                        }
                    }
                }
//                Thread.sleep(1000)
            }
            return geodeMax.toLong()
        }
    }

    enum class Material {
        ORE,
        CLAY,
        OBSIDIAN,
        GEODE
    }

    private fun part1(lines: List<String>): Any {
        val blueprints = parseBlueprints(lines)
        return blueprints.map { it.id * it.findMostGeode(24) }.sum()
    }

    private fun parseBlueprints(lines: List<String>): List<Blueprint> {
        val blueprints = lines.map { line ->
            val parts = line.split(" ", ":")
            val costs = mapOf(
                Material.ORE to mapOf(Material.ORE to parts[7].toInt()),
                Material.CLAY to mapOf(Material.ORE to parts[13].toInt()),
                Material.OBSIDIAN to mapOf(Material.ORE to parts[19].toInt(), Material.CLAY to parts[22].toInt()),
                Material.GEODE to mapOf(Material.ORE to parts[28].toInt(), Material.OBSIDIAN to parts[31].toInt()),
            )
            Blueprint(parts[1].toInt(), costs)
        }
        return blueprints
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

