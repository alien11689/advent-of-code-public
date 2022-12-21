package pl.touk.dpr.aoc2021

import java.util.Stack
import kotlin.math.absoluteValue

object Day19 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/19/input.txt")
        part1And2(lines).forEach(::println)
    }

    private fun part1And2(lines: List<String>): List<Int> {
        val fullResult = mutableListOf<Int>()

        val scanners = readScanners(lines)

        val distancesMaps = scanners.map { it.mapDistance() }

        val beaconMappings = mutableMapOf<Pair<Int, Beacon>, Set<Pair<Int, Beacon>>>()

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

        val commonBeacons = res.flatten().toSet()
//        println(commonBeacons)
        val allBeacons = scanners.flatMapIndexed { idx, scanner -> scanner.beacons.map { Pair(idx, it) } }.toSet()

        val part1Result = res.size + allBeacons.count { it !in commonBeacons }

        fullResult.add(part1Result)
        fullResult.add(findCenters(scanners, res))

        return fullResult
    }

    private fun findCenters(scanners: MutableList<Scanner>, res: MutableSet<Set<Pair<Int, Beacon>>>): Int {
        var beaconsMatching = res.map { it }
        val centers = mutableMapOf<Int, Beacon>()
        val rotations = mutableMapOf<Int, Int>()
        centers[0] = Beacon(0, 0, 0)
        rotations[0] = 0

        var oldSize = -1
        while (centers.size != oldSize) {
            oldSize = centers.size
            for (j in scanners.indices) {
                if (j in centers.keys) {
                    continue
                }
                val zeroAndOne = beaconsMatching.filter { theSame -> theSame.count { it.first in listOf(0, j) } == 2 }
                    .map { it.filter { it.first in listOf(0, j) } }
                    .take(2)
                val onlyFrom0 = zeroAndOne.map { it.filter { it.first == 0 } }.flatten()
                val onlyFromJ = zeroAndOne.map { it.filter { it.first == j } }.flatten()
                if (onlyFromJ.size != 2) {
                    continue
                }
                val zeroExpectedVector = onlyFrom0.map { it.second }.reduce { acc, cur -> Beacon(acc.x - cur.x, acc.y - cur.y, acc.z - cur.z) }
                val rotations1 = onlyFromJ.get(0).second.allRotations()
                val rotations2 = onlyFromJ.get(1).second.allRotations()
                for (rotationId in rotations1.indices) {
                    val vector = listOf(rotations1[rotationId], rotations2[rotationId]).reduce { acc, cur -> Beacon(acc.x - cur.x, acc.y - cur.y, acc.z - cur.z) }
                    if (vector == zeroExpectedVector) {
//                        println("Rotation $rotationId matches")
//                        println("${rotations1[rotationId]}")
                        val center = onlyFrom0.first().second - rotations1[rotationId]
//                        println("Center is $center")
                        centers[j] = center
                        rotations[j] = rotationId
                        break
                    }
                }
                beaconsMatching = beaconsMatching.filter { it.filter { it.first !in centers.keys }.count() > 0 }
                beaconsMatching = beaconsMatching.map { it.map { if (it.first in rotations.keys) Pair(0, it.second.allRotations()[rotations[it.first]!!] + centers[it.first]!!) else it }.toSet() }
            }
        }
//        println(centers)
//        println(beaconsMatching)
        val sensorsCenters = centers.values.toList()
        var maxManhattan = -1
        for (i in sensorsCenters.indices) {
            for (j in sensorsCenters.indices) {
                if (i < j) {
                    val manh = manhattan(sensorsCenters[i], sensorsCenters[j])
                    if(manh > maxManhattan){
                        maxManhattan = manh
                    }
                }
            }
        }
        return maxManhattan
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

    data class Beacon(val x: Int, val y: Int, val z: Int) {
        fun allRotations(): List<Beacon> {
            val all = mutableListOf<Beacon>()
            listOf(0, 90, 180, 270).forEach { rx ->
                listOf(0, 90, 180, 270).forEach { ry ->
                    listOf(0, 90, 180, 270).forEach { rz ->
                        all.add(this.rotateX(rx).rotateY(ry).rotateZ(rz))
                    }
                }
            }
            return all
        }

        fun rotateZ(degree: Int): Beacon {
            var sinTheta = if (degree == 90) 1 else if (degree == 270) -1 else 0;
            var cosTheta = if (degree == 180) -1 else if (degree == 0) 1 else 0;
            return Beacon(x * cosTheta - y * sinTheta, y * cosTheta + x * sinTheta, z)
        }

        fun rotateX(degree: Int): Beacon {
            var sinTheta = if (degree == 90) 1 else if (degree == 270) -1 else 0;
            var cosTheta = if (degree == 180) -1 else if (degree == 0) 1 else 0;
            return Beacon(x, y * cosTheta - z * sinTheta, z * cosTheta + y * sinTheta)
        }

        fun rotateY(degree: Int): Beacon {
            var sinTheta = if (degree == 90) 1 else if (degree == 270) -1 else 0;
            var cosTheta = if (degree == 180) -1 else if (degree == 0) 1 else 0;
            return Beacon(x * cosTheta + z * sinTheta, y, z * cosTheta - x * sinTheta)
        }

        operator fun minus(beacon: Beacon): Beacon {
            return Beacon(x - beacon.x, y - beacon.y, z - beacon.z)
        }

        operator fun plus(beacon: Beacon): Beacon {
            return Beacon(x + beacon.x, y + beacon.y, z + beacon.z)
        }
    }

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
}

