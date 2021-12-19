package pl.touk.dpr.aoc2021

import java.util.Stack
import kotlin.math.absoluteValue

object Day19 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/19/input2.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val scanners = readScanners(lines)

        val distancesMaps = scanners.map { it.mapDistance() }

//        val hasDistUsedMoreThanOne = distancesMaps.filter { it.second.values.count { it.size > 1 } > 0 }.count()
//        println("hasDistUsedMoreThanOne: $hasDistUsedMoreThanOne")

        val beaconMappings = mutableMapOf<Pair<Int, Beacon>, Set<Pair<Int, Beacon>>>()
//        scanners[0].beacons.forEach {
//            resultBeacons.add(mutableSetOf(Pair(0, it)))
//        }

        for (i in distancesMaps.indices) {
            val m1 = distancesMaps[i]
            for (j in distancesMaps.indices) {
                if (i < j) {
                    val m2 = distancesMaps[j]
                    val commonDistances = m1.keys.filter { it in m2.keys }
                    val countOverlappingKeys = commonDistances.count()
                    if (countOverlappingKeys > 0) {
//                        println("Scanner ${i} and ${j} have $countOverlappingKeys overlapping distances")
                        val commonBeacons1 = m1.filter { it.key in commonDistances }.map { it.value }.flatten().flatten().toSet()
                        val commonBeacons2 = m1.filter { it.key in commonDistances }.map { it.value }.flatten().flatten().toSet()
//                        println("Common beacons count is ${commonBeacons1.size}")
//                        println("Common beacons is ${commonBeacons1}")
                        for (cb in commonBeacons1) {
                            val x = m1.filter { it.key in commonDistances }.filter { it.value.count { it.contains(cb) } > 0 }
                            val two = x.toList().take(2).toMap()
                            val matchingInSecond = m2.filter { it.key in two.keys }.map { it.value }.flatten().flatten()
                                .groupBy { it }.maxByOrNull { it.value.count() }!!.key
//                            println("$cb is second set ${matchingInSecond}")
                            val fullBeacon1 = Pair(i, cb)
                            val fullBeacon2 = Pair(j, matchingInSecond)
                            val cb1Mappings = beaconMappings[fullBeacon1] ?: emptySet()
                            beaconMappings[fullBeacon1] = cb1Mappings + fullBeacon2
//                            val ms = mutableSetOf(fullBeacon1, fullBeacon2)
//                            println("$cb in $x")
//                            println("$cb in circa ${x.count()}")
//                            return -2
                        }
                    }
                }
            }
        }

//        println(beaconMappings)

        val checkedFullBeacons = mutableSetOf<Pair<Int, Beacon>>()
        val res = mutableSetOf<Set<Pair<Int, Beacon>>>()

        beaconMappings.forEach { key, value ->
            if (key !in checkedFullBeacons) {
                val ms = mutableSetOf<Pair<Int, Beacon>>()
                val s = Stack<Pair<Int, Beacon>>()
                s.push(key)
                while (s.isNotEmpty()) {
                    val fullBeacon = s.pop()
                    ms.add(fullBeacon)
                    checkedFullBeacons.add(fullBeacon)
                    val used = beaconMappings[fullBeacon] ?: emptySet()
//                    println("$fullBeacon is used in $used")
                    used.filter { it !in checkedFullBeacons }.forEach {
                        s.push(it)
                    }
                }
                res += ms.toSet()
            }
        }

//        res.forEach(::println)

        val commonBeacons = res.flatten().toSet()
//        println(commonBeacons)
        val allBeacons = scanners.flatMapIndexed { idx, scanner -> scanner.beacons.map { Pair(idx, it) } }.toSet()

        val part1Result = res.size + allBeacons.count { it !in commonBeacons }
        println("Part1: $part1Result")

        
        return part1Result
    }

    private fun readScanners(lines: List<String>): MutableList<Scanner> {
        var id: String? = null
        var beacons = mutableListOf<Beacon>()
        val scanners = mutableListOf<Scanner>()
        lines.forEach { line ->
            if (line.startsWith("---")) {
                if (id != null) {
                    scanners += Scanner(id!!, beacons)
                    beacons = mutableListOf()
                }
                id = line.split(" ")[2]
            } else {
                val points = line.split(",").map { it.toInt() }
                beacons.add(Beacon(points[0], points[1], points[2]))
            }
        }
        scanners += Scanner(id!!, beacons)
        return scanners
    }

    data class Beacon(val x: Int, val y: Int, val z: Int)

    data class Scanner(val id: String, val beacons: List<Beacon>) {
        fun mapDistance(): Map<Set<Int>, Set<Set<Beacon>>> {
            val mm = mutableMapOf<Set<Int>, MutableSet<Set<Beacon>>>()
            for (i in beacons.indices) {
                val b1 = beacons[i]
                for (j in beacons.indices) {
                    if (i < j) {
                        val b2 = beacons[j]
                        val dists = distance(b1, b2)
                        val oldSet = mm[dists] ?: mutableSetOf()
                        oldSet.add(setOf(b1, b2))
                        mm[dists] = oldSet
                    }
                }
            }
            return mm
        }
    }

    fun manhattan(a: Beacon, b: Beacon): Int {
        return (a.x - b.x).absoluteValue + (a.y - b.y).absoluteValue + (a.z - b.z).absoluteValue
    }

    fun distance(a: Beacon, b: Beacon): Set<Int> {
        return setOf((a.x - b.x).absoluteValue, (a.y - b.y).absoluteValue, (a.z - b.z).absoluteValue)
    }

    private fun part2(lines: List<String>): Any {
        return -1
    }
}

