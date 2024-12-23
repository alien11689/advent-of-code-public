package dpr.aoc2023

import dpr.commons.Util
import org.sosy_lab.common.ShutdownManager
import org.sosy_lab.common.configuration.Configuration
import org.sosy_lab.common.log.BasicLogManager
import org.sosy_lab.java_smt.SolverContextFactory
import org.sosy_lab.java_smt.SolverContextFactory.Solvers
import org.sosy_lab.java_smt.api.Model
import org.sosy_lab.java_smt.api.SolverContext.ProverOptions
import java.io.BufferedReader
import java.io.InputStreamReader

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

        val resultSmt = solveWithSmt(points)
//        val resultZ3 = solveWithZ3InPython(points)
//        if (resultSmt != resultZ3) {
//            throw RuntimeException("Solutions from solvers are different - SMT: $resultSmt, Z3: $resultZ3")
//        }
        return resultSmt
    }

    private fun solveWithZ3InPython(points: List<PointAndVector>): String {
        val processBuilder = ProcessBuilder()
        val tmpFile = kotlin.io.path.createTempFile().toFile()
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

    private fun solveWithSmt(points: List<PointAndVector>): String {
        val config = Configuration.defaultConfiguration()
        val logger = BasicLogManager.create(config)
        val shutdown = ShutdownManager.create()
        val context = SolverContextFactory.createSolverContext(
            config, logger, shutdown.notifier, Solvers.PRINCESS
        )

        val fmgr = context.formulaManager
        val bmgr = fmgr.booleanFormulaManager
        val f = fmgr.integerFormulaManager

        val x = f.makeVariable("x")
        val y = f.makeVariable("y")
        val z = f.makeVariable("z")
        val dx = f.makeVariable("dx")
        val dy = f.makeVariable("dy")
        val dz = f.makeVariable("dz")
        val t1 = f.makeVariable("t1")
        val t2 = f.makeVariable("t2")
        val t3 = f.makeVariable("t3")

        val x1 = f.makeNumber(points[0].p.x)
        val y1 = f.makeNumber(points[0].p.y)
        val z1 = f.makeNumber(points[0].p.z)
        val dx1 = f.makeNumber(points[0].speed.dx.toDouble())
        val dy1 = f.makeNumber(points[0].speed.dy.toDouble())
        val dz1 = f.makeNumber(points[0].speed.dz.toDouble())

        val x2 = f.makeNumber(points[1].p.x)
        val y2 = f.makeNumber(points[1].p.y)
        val z2 = f.makeNumber(points[1].p.z)
        val dx2 = f.makeNumber(points[1].speed.dx.toDouble())
        val dy2 = f.makeNumber(points[1].speed.dy.toDouble())
        val dz2 = f.makeNumber(points[1].speed.dz.toDouble())

        val x3 = f.makeNumber(points[2].p.x)
        val y3 = f.makeNumber(points[2].p.y)
        val z3 = f.makeNumber(points[2].p.z)
        val dx3 = f.makeNumber(points[2].speed.dx.toDouble())
        val dy3 = f.makeNumber(points[2].speed.dy.toDouble())
        val dz3 = f.makeNumber(points[2].speed.dz.toDouble())

        val constraint = bmgr.and(
            f.greaterThan(t1, f.makeNumber(0)),
            f.greaterThan(t2, f.makeNumber(0)),
            f.greaterThan(t3, f.makeNumber(0)),
            f.equal(f.add(x, f.multiply(dx, t1)), f.add(x1, f.multiply(dx1, t1))),
            f.equal(f.add(x, f.multiply(dx, t2)), f.add(x2, f.multiply(dx2, t2))),
            f.equal(f.add(x, f.multiply(dx, t3)), f.add(x3, f.multiply(dx3, t3))),
            f.equal(f.add(y, f.multiply(dy, t1)), f.add(y1, f.multiply(dy1, t1))),
            f.equal(f.add(y, f.multiply(dy, t2)), f.add(y2, f.multiply(dy2, t2))),
            f.equal(f.add(y, f.multiply(dy, t3)), f.add(y3, f.multiply(dy3, t3))),
            f.equal(f.add(z, f.multiply(dz, t1)), f.add(z1, f.multiply(dz1, t1))),
            f.equal(f.add(z, f.multiply(dz, t2)), f.add(z2, f.multiply(dz2, t2))),
            f.equal(f.add(z, f.multiply(dz, t3)), f.add(z3, f.multiply(dz3, t3))),
        )

        val result = context.newProverEnvironment(ProverOptions.GENERATE_MODELS).use { prover ->
            prover.addConstraint(constraint)
            val isUnsat = prover.isUnsat
            if (isUnsat) {
                throw RuntimeException("No solution")
            }
            val model: Model = prover.model
            model.evaluate(x)!!.add(model.evaluate(y)).add(model.evaluate(z))
        }
        return result.toString()
    }
}

