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
        override fun compareTo(other: State): Int = other.time.compareTo(time)
        fun nexts(robotCosts: Map<Material, Map<Material, Int>>): List<State> {
            val options = mutableListOf<State>()
            val harvestedMaterials = merge(materials, robots)
            options.add(copy(time - 1, materials = harvestedMaterials))
            canAfford(robotCosts).forEach { robotToBuild ->
                val (material, cost) = robotToBuild
//                println("I can by $robotToBuild")
                options.add(copy(time - 1, materials = minus(harvestedMaterials, cost), merge(robots, mapOf(material to 1))))
            }
            return options
        }

        private fun canAfford(robotCosts: Map<Material, Map<Material, Int>>): List<Pair<Material, Map<Material, Int>>> {
            return robotCosts.mapNotNull { e ->
//                println("Can I buy $e having $materials?")
                val cost = e.value
                if (cost.all { materials[it.key]!! >= it.value }) {
//                    println("Yes")
                    e.toPair()
                } else null
            }
        }
    }

    private fun merge(first: Map<Material, Int>, second: Map<Material, Int>): Map<Material, Int> =
        Material.values().associateWith { (first[it] ?: 0) + (second[it] ?: 0) }

    private fun minus(first: Map<Material, Int>, second: Map<Material, Int>): Map<Material, Int> =
        Material.values().associateWith { (first[it] ?: 0) - (second[it] ?: 0) }

    data class Blueprint(val id: Int, val robotCosts: Map<Material, Map<Material, Int>>) {
        fun findMostGeode(turns: Int): Long {
            val memory = mutableSetOf<State>()
            var minTurns = turns
            var geodeMax = 0
            val pq = PriorityQueue<State>()
            pq.add(State(turns, Material.values().associateWith { 0 }, mapOf(Material.ORE to 1, Material.CLAY to 0, Material.OBSIDIAN to 0, Material.GEODE to 0)))
            while (pq.isNotEmpty()) {
                val cur = pq.poll()
                if (cur.time < minTurns) {
                    minTurns = cur.time
                    println("Anylizing $minTurns, pq size: ${pq.size}")
//                    println("Cur $cur")
//                    println("PQ:")
//                    while (pq.isNotEmpty()) {
//                        println(" ${pq.poll()}")
//                    }
//                    return -1
                }
                cur.nexts(robotCosts).forEach {
                    if (it.time == 0) {
                        val geodeCount = it.materials[Material.GEODE] ?: 0
                        if (geodeMax < geodeCount) {
                            geodeMax = geodeCount
                        }
                    } else {
                        if(it !in memory){
                            memory.add(it)
                            pq.offer(it)
                        }
                    }
                }
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

