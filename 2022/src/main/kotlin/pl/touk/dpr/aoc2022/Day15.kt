package pl.touk.dpr.aoc2022

import kotlin.math.absoluteValue

object Day15 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/15/input.txt")
        println("Part 1:")
        println(part1(Util.getNotEmptyLinesFromFile("/15/test1.txt"), 10))
        println(part1(lines, 2000000))
        println("Part 2:")
        println(part2(Util.getNotEmptyLinesFromFile("/15/test1.txt")))
        println(part2(lines))
    }

    data class Point(val x: Int, val y: Int) {
        fun manhattan(other: Point): Int = (other.x - x).absoluteValue + (other.y - y).absoluteValue
    }

    data class SensorBeacon(val sensor: Point, val beacon: Point, val dist: Int)

    private fun part1(lines: List<String>, interestingRow: Int): Any {
        val sensors2Beacon = readInput(lines)
        val sensors = sensors2Beacon.map { it.sensor }
        val beacons = sensors2Beacon.map { it.beacon }
        val maxDist = sensors2Beacon.map { it.dist}.max()
        val minX = sensors.map { it.x }.min() - maxDist
        val maxX = sensors.map { it.x }.max() + maxDist


        var positions = 0
        for (x in minX..maxX) {
            val possibleBeacon = Point(x, interestingRow)
            if (possibleBeacon in beacons) {
                continue
            }
            val count = sensors2Beacon.count {
                val localDist = possibleBeacon.manhattan(it.sensor)
                localDist <= it.dist
            }
            if (count > 0) {
                ++positions
//                println("Checked $possibleBeacon - in scope")
            } else {
//                println("Checked $possibleBeacon - outside")
            }
        }
        return positions
    }

    private fun readInput(lines: List<String>) = lines.map {
        val parts = it.split(",", ":", "=")
//        for (i in parts.indices) {
//            println("$i -> ${parts[i]}")
//        }
        val sensor = Point(parts[1].toInt(), parts[3].toInt())
        val beacon = Point(parts[5].toInt(), parts[7].toInt())
        SensorBeacon(sensor, beacon, sensor.manhattan(beacon))
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

