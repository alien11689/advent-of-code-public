package dpr.aoc2023

import dpr.commons.Util
import java.io.BufferedReader
import java.io.InputStreamReader
import kotlin.io.path.createTempFile

object Day24 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/24/input.txt")
        val min: Long = 200000000000000
        val max: Long = 400000000000000
//        val lines = Util.getNotEmptyLinesFromFile("/24/test1.txt")
//        val min: Long = 7
//        val max: Long = 27
        println(part1(lines, min, max))
        println(part2(lines))
    }

    data class Vector(val dx: Long, val dy: Long, val dz: Long)
    data class Point(val x: Double, val y: Double, val z: Double)

    data class Line2d(val a: Double, val b: Double)

    data class PointAndVector(val p: Point, val speed: Vector) {
        private val secondPoint = Point(p.x + speed.dx, p.y + speed.dy, p.z + speed.dz)
        val lineXY = calculateLine(p, secondPoint)

        private fun calculateLine(p1: Point, p2: Point): Line2d {
            val a = (p1.y - p2.y) * 1.0 / (p1.x - p2.x)
            val b = p1.y - a * p1.x
            return Line2d(a, b)
        }
    }

    private fun part1(lines: List<String>, min: Long, max: Long): Any {
        val points = readPointsAndVectors(lines)
        //        points.forEach { println(it) }
//        points.forEach { println(it.line) }
        var res = 0
        for (i in points.indices) {
            for (j in (i + 1)..<points.size) {
                val (a, b) = points[i].lineXY
                val (a1, b1) = points[j].lineXY
                val x = (b1 - b) / (a - a1)
                val y = a * x + b
                val intersection = Point(x, y, 0.0)
//                println("Intersaction of ${points[i]} and ${points[j]} is $intersection")
                if (intersection.x >= min && intersection.x <= max && intersection.y >= min && intersection.y <= max) {
//                    println("Point $intersection in range $min..$max")
                    val pwv1 = points[i]
                    val pwv2 = points[j]
                    val t1 = (intersection.x - pwv1.p.x) / pwv1.speed.dx
                    val t2 = (intersection.x - pwv2.p.x) / pwv2.speed.dx
                    if (t1 > 0 && t2 > 0) {
                        ++res
                    }
                }
            }
        }

        return res
    }

    private fun readPointsAndVectors(lines: List<String>): List<PointAndVector> {
        val points = lines.map {
            val parts = it.replace(" ", "").split(Regex("[,@]+"))
            PointAndVector(
                Point(parts[0].toDouble(), parts[1].toDouble(), parts[2].toDouble()),
                Vector(parts[3].toLong(), parts[4].toLong(), parts[5].toLong())
            )
        }
        return points
    }

    private fun part2(lines: List<String>): Any {
        val points = readPointsAndVectors(lines)

        // Solve equation:
        // t1 > 0
        // t2 > 0
        // t3 > 0
        // x + dx * t1 == x1 + dx1 * t1
        // x + dx * t2 == x2 + dx2 * t2
        // x + dx * t3 == x3 + dx3 * t3
        // y + dy * t1 == y1 + dy1 * t1
        // y + dy * t2 == y2 + dy2 * t2
        // y + dy * t3 == y3 + dy3 * t3
        // z + dz * t1 == z1 + dz1 * t1
        // z + dz * t2 == z2 + dz2 * t2
        // z + dz * t3 == z3 + dz3 * t3

        // Run z3 solver from resources
        val processBuilder = ProcessBuilder()
        val tmpFile = createTempFile().toFile()
        tmpFile.deleteOnExit()
        javaClass.getResourceAsStream("/24/day24.py").use { input ->
            tmpFile.outputStream().use { output ->
                input!!.copyTo(output)
            }
        }
        val command =
            listOf("python3", tmpFile.absolutePath) +
                points.take(3)
                    .map { pwv -> listOf(pwv.p.x, pwv.p.y, pwv.p.z, pwv.speed.dx, pwv.speed.dy, pwv.speed.dz).map { it.toLong().toString() } }
                    .flatten()
        processBuilder.command(command)
        val process = processBuilder.start()
        val reader = BufferedReader(InputStreamReader(process.inputStream))
        return reader.readText().trim()
    }
}

