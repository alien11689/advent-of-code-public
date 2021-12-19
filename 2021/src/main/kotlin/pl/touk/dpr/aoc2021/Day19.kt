package pl.touk.dpr.aoc2021

import java.util.Stack
import kotlin.math.absoluteValue

object Day19 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/19/input2.txt")
        part1(lines)
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
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
        println("Part1: $part1Result")

        val i = 0
        val j = 3
        val zeroAndOne = res.filter { theSame -> theSame.count { it.first in listOf(i, j) } == 2 }
            .map { it.filter { it.first in listOf(i, j) } }
            .take(2)
        val onlyFrom0 = zeroAndOne.map { it.filter { it.first == i } }.flatten()
        val onlyFrom1 = zeroAndOne.map { it.filter { it.first == j } }.flatten()
        println(zeroAndOne)
        val zeroExpectedVector = onlyFrom0.map { it.second }.reduce { acc, cur -> Beacon(acc.x - cur.x, acc.y - cur.y, acc.z - cur.z) }
        println("$onlyFrom0 has dist $zeroExpectedVector")
        println("$onlyFrom1 has dist ${onlyFrom1.map { it.second }.reduce { acc, cur -> Beacon(acc.x - cur.x, acc.y - cur.y, acc.z - cur.z) }}")
        val rotations1 = onlyFrom1.get(0).second.allRotations()
        val rotations2 = onlyFrom1.get(1).second.allRotations()
        for (rotationId in rotations1.indices) {
            val vector = listOf(rotations1[rotationId], rotations2[rotationId]).reduce { acc, cur -> Beacon(acc.x - cur.x, acc.y - cur.y, acc.z - cur.z) }
            if (vector == zeroExpectedVector) {
                println("Rotation $rotationId matches")
                println("${rotations1[rotationId]}")
                println("Center is ${onlyFrom0.first().second - rotations1[rotationId]}")
                break
            }
        }

//        res.forEach(::println)

//        [(0, Beacon(x=423, y=-701, z=434)),//(2, Beacon(x=682, y=-795, z=504)),
//        [(0, Beacon(x=459, y=-707, z=401)),//(2, Beacon(x=646, y=-828, z=498)),

//        val b = Beacon(1, 2, 3)
//        println(b.allRotations())
//        println(b.allRotations().size)
//        println(b.allRotations().toSet().size)

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

    private fun part2(lines: List<String>): Any {
        return -1
    }
}

