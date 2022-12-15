package pl.touk.dpr.aoc2022

import kotlin.math.absoluteValue

object Day15 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/15/input.txt")
        println("Part 1:")
//        println(part1(Util.getNotEmptyLinesFromFile("/15/test1.txt"), 10))
        println(part1(lines, 2000000))
        println("Part 2:")
        println(part2(Util.getNotEmptyLinesFromFile("/15/test1.txt"), 20))
        println(part2(lines, 4000000))
    }

    data class Point(val x: Int, val y: Int) {
        fun manhattan(other: Point): Int = (other.x - x).absoluteValue + (other.y - y).absoluteValue
    }

    data class SensorBeacon(val sensor: Point, val beacon: Point, val dist: Int)

    private fun part1(lines: List<String>, interestingRow: Int): Any {
        val sensors2Beacon = readInput(lines)
        val sensors = sensors2Beacon.map { it.sensor }
        val beacons = sensors2Beacon.map { it.beacon }
        val maxDist = sensors2Beacon.map { it.dist }.max()
        val minX = sensors.map { it.x }.min() - maxDist
        val maxX = sensors.map { it.x }.max() + maxDist


        var positions = 0
        for (x in minX..maxX) {
            val possibleBeacon = Point(x, interestingRow)
            if (possibleBeacon in beacons) {
                continue
            }
            val sensorCloserExist = sensors2Beacon.any {
                val localDist = possibleBeacon.manhattan(it.sensor)
                localDist <= it.dist
            }
            if (sensorCloserExist) {
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
        val sensor = Point(parts[1].toInt(), parts[3].toInt())
        val beacon = Point(parts[5].toInt(), parts[7].toInt())
        SensorBeacon(sensor, beacon, sensor.manhattan(beacon))
    }

    private fun part2(lines: List<String>, maxCoord: Int): Any {
        val sensors2Beacon = readInput(lines)
        val sensors = sensors2Beacon.map { it.sensor }
        val beacons = sensors2Beacon.map { it.beacon }
//        println("Max dist: ${sensors2Beacon.maxOfOrNull { it.dist }}")
//        println("X beacon range is ${sensors.minOfOrNull { it.x }}..${sensors.maxOfOrNull { it.x }}")
//        println("Y beacon range is ${sensors.minOfOrNull { it.y }}..${sensors.maxOfOrNull { it.y }}")
//        println("X sensor range is ${beacons.minOfOrNull { it.x }}..${beacons.maxOfOrNull { it.x }}")
//        println("Y sensor range is ${beacons.minOfOrNull { it.y }}..${beacons.maxOfOrNull { it.y }}")
        val minX = 0
        val maxX = maxCoord
        val minY = 0
        val maxY = maxCoord
        val points = mutableSetOf<Point>()

//        sensors2Beacon.forEach {
//            println("Distance ${it.dist}")
//        }

//        for (y in minY..maxY) {
//            println("Adding row $y")
//            points.addAll((minX..maxX).map { Point(it, y) })
//        }

        var y = minY

        while (y <= maxY) {
            println("Checking row $y")
            var x = minX
            while (x <= maxX) {
                val possibleBeacon = Point(x, y)
                val sensorsSeeing = sensors2Beacon.filter {
                    val localDist = possibleBeacon.manhattan(it.sensor)
                    localDist <= it.dist
                }
                if (sensorsSeeing.isEmpty()) {
                    return possibleBeacon.x.toLong() * 4000000 + possibleBeacon.y
                }
                ++x
            }
            ++y
        }
        throw RuntimeException()
//        val res = Point(0, 0)// result
//        return res.x.toLong() * 4000000 + res.y
    }
}

