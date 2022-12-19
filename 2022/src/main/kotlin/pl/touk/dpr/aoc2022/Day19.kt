package pl.touk.dpr.aoc2022

import java.util.PriorityQueue

object Day19 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/19/input.txt")
        println("Part 1:")
//        println(part1(Util.getNotEmptyLinesFromFile("/19/test1.txt")))
//        println(part1(lines))
        println("Part 2:")
        println(part2(Util.getNotEmptyLinesFromFile("/19/test1.txt")))
        // now second example is 61 but should 62
        println(part2(lines))
        //29480 is too low
    }

    data class State(val time: Int, val materials: Map<Material, Int>, val robots: Map<Material, Int>) : Comparable<State> {
        val materialScore =
            (materials[Material.ORE] ?: 0) + 1000L * ((materials[Material.CLAY] ?: 0) + 1000L * ((materials[Material.OBSIDIAN] ?: 0) + 1000L * (materials[Material.GEODE] ?: 0)))

        val robotsScore =
            (robots[Material.ORE] ?: 0) + 1000L * ((robots[Material.CLAY] ?: 0) + 1000L * ((robots[Material.OBSIDIAN] ?: 0) + 1000L * (robots[Material.GEODE] ?: 0)))

//        override fun compareTo(other: State): Int =
//            if (other.robotsScore == robotsScore) {
//                if (time == other.time) {
//                    other.materialScore.compareTo(materialScore)
//                } else other.time.compareTo(time)
//            } else other.robotsScore compareTo robotsScore

//        override fun compareTo(other: State): Int =
//            if (other.materialScore == materialScore) {
//                if (time == other.time) {
//                    other.robotsScore.compareTo(robotsScore)
//                } else other.time.compareTo(time)
//            } else other.materialScore compareTo materialScore

        override fun compareTo(other: State): Int = if (other.possibleGeodes == possibleGeodes)
            if (other.possibleObsidians == possibleObsidians) other.possibleClays.compareTo(possibleClays)
            else other.possibleObsidians.compareTo(possibleObsidians)
        else other.possibleGeodes.compareTo(possibleGeodes)

        fun nexts(robotCosts: Map<Material, Map<Material, Int>>, geodeMax: Int): List<State> {
            val options = mutableListOf<State>()
            if (time < 5 && (robots[Material.CLAY] ?: 0) == 0 || time < 4 && (robots[Material.OBSIDIAN] ?: 0) == 0 || time < 3 && (robots[Material.GEODE] ?: 0) == 0) {
                return options
            }
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

        val possibleGeodes: Int = (materials[Material.GEODE] ?: 0) + ((time - 1) downTo 0).sumOf { (robots[Material.GEODE] ?: 0) + it + 1 }
        val possibleObsidians: Int = (materials[Material.OBSIDIAN] ?: 0) + ((time - 1) downTo 0).sumOf { (robots[Material.OBSIDIAN] ?: 0) + it + 1 }
        val possibleClays: Int = (materials[Material.CLAY] ?: 0) + ((time - 1) downTo 0).sumOf { (robots[Material.CLAY] ?: 0) + it + 1 }
    }

    private fun merge(first: Map<Material, Int>, second: Map<Material, Int>): Map<Material, Int> =
        Material.values().associateWith { (first[it] ?: 0) + (second[it] ?: 0) }

    private fun minus(first: Map<Material, Int>, second: Map<Material, Int>): Map<Material, Int> =
        Material.values().associateWith { (first[it] ?: 0) - (second[it] ?: 0) }

    private fun times(first: Map<Material, Int>, repeat: Int): Map<Material, Int> =
        Material.values().associateWith { (first[it] ?: 0) * repeat }

    data class Blueprint(val id: Int, val robotCosts: Map<Material, Map<Material, Int>>) {
        fun findMostGeode(turns: Int): Long {
            val best = mutableMapOf<Pair<Map<Material, Int>, Map<Material, Int>>, Int>()
            val memory = mutableSetOf<State>()
            var geodeMax = 0
            val pq = PriorityQueue<State>()
            pq.add(State(turns, emptyMap(), mapOf(Material.ORE to 1)))
            while (pq.isNotEmpty()) {
                val cur = pq.poll()
//                println("Analyzing $cur, pq size: ${pq.size}, max geode: $geodeMax, possible: ${cur.possibleGeodes()}")
                if (cur.possibleGeodes <= geodeMax) {
                    continue
                }
                cur.nexts(robotCosts, geodeMax).forEach {
                    if (it.time == 0) {
                        val geodeCount = it.materials[Material.GEODE] ?: 0
                        if (geodeMax < geodeCount) {
                            geodeMax = geodeCount
                            println("New Max geode $geodeMax -> $it")
                        }
                    } else {
                        if (it !in memory) {
                            memory.add(it)
                            val key = it.materials to it.robots
                            if (key !in best) {
                                best[key] = it.time
                                pq.offer(it)
                            } else {
                                val bestTime = best[key]!!
                                if (bestTime < it.time) {
                                    pq.offer(it)
                                    best[key] = it.time
                                }
                            }
                        }
                    }
                }
            }
            println("Calculated for $id -> $geodeMax")
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
        return blueprints.sumOf { it.id * it.findMostGeode(24) }
    }

    private fun parseBlueprints(lines: List<String>): List<Blueprint> {
        return lines.map { line ->
            val parts = line.split(" ", ":")
            val costs = mapOf(
                Material.ORE to mapOf(Material.ORE to parts[7].toInt()),
                Material.CLAY to mapOf(Material.ORE to parts[13].toInt()),
                Material.OBSIDIAN to mapOf(Material.ORE to parts[19].toInt(), Material.CLAY to parts[22].toInt()),
                Material.GEODE to mapOf(Material.ORE to parts[28].toInt(), Material.OBSIDIAN to parts[31].toInt()),
            )
            Blueprint(parts[1].toInt(), costs)
        }
    }

    private fun part2(lines: List<String>): Any {
        val blueprints = parseBlueprints(lines).take(3)
        return blueprints.map { it.findMostGeode(32) }.reduce { acc, cur -> acc * cur }
    }
}

