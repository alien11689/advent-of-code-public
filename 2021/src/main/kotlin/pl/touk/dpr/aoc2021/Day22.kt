package pl.touk.dpr.aoc2021

typealias Cubicle = Triple<IntRange, IntRange, IntRange>

object Day22 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/22/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val initPoints = setOf<Triple<Int, Int, Int>>()
        val instructions = readInstructions(lines)
        val cubes = instructions
            .filter { it.isInit() }
            .fold(initPoints) { acc, instr ->
                val newS = if (instr.oper == Oper.off) {
                    acc.filterNot { it.first in instr.xRange && it.second in instr.yRange && it.third in instr.zRange }.toSet()
                } else {
                    acc + instr.generateTriples()
                }
                println("Volume is ${newS.size}")
                newS
            }
        return cubes.size
    }

    private fun part2(lines: List<String>): Any {
        var cubicles = setOf<Cubicle>()
        val instructions = readInstructions(lines)
        instructions
//            .filter { it.isInit() }
            .forEachIndexed { idx, instr ->
                if (idx == 0) {
                    cubicles = setOf(instr.cubicle())
                } else {
                    val curCub = instr.cubicle()
                    cubicles = cubicles.flatMap { oldCubicle ->
                        fullSplit(oldCubicle, curCub)
                    }.toSet()
                    if (instr.oper == Oper.on) {
                        cubicles = cubicles.filter { !contains(curCub, it) }.toSet() + curCub
                    } else {
                        cubicles = cubicles.filter { !contains(curCub, it) }.toSet()
                    }
                }
                println("Cubicles size is ${cubicles.size} and volume is ${cubicles.sumOf { volume(it) }}")
            }
        return cubicles.sumOf { volume(it) }
    }

    private fun readInstructions(lines: List<String>) = lines.map { it.split(" ", ",", ".", "=") }
        .mapIndexed { idx, it ->
            Instr(
                if (it[0] == "on") Oper.on else Oper.off,
                it[2].toInt()..it[4].toInt(),
                it[6].toInt()..it[8].toInt(),
                it[10].toInt()..it[12].toInt(),
                idx
            )
        }

    enum class Oper { on, off }

    data class Instr(val oper: Oper, val xRange: IntRange, val yRange: IntRange, val zRange: IntRange, val id: Int = 0) {
        fun generateTriples(): Set<Triple<Int, Int, Int>> {
            val points = mutableSetOf<Triple<Int, Int, Int>>()
            for (x in xRange) {
                for (y in yRange) {
                    for (z in zRange) {
                        points.add(Triple(x, y, z))
                    }
                }
            }
            return points
        }

        fun size(): Long {
            return (xRange.last - xRange.first + 1).toLong() * (yRange.last - yRange.first + 1).toLong() * (zRange.last - zRange.first + 1).toLong()
        }

        fun isInit(): Boolean {
            return xRange.first >= -50 && xRange.last <= 50
                    && yRange.first >= -50 && yRange.last <= 50
                    && zRange.first >= -50 && zRange.last <= 50
        }

        fun cubicle(): Cubicle = Cubicle(xRange, yRange, zRange)
    }

//    fun overlap(c1: Cubicle, other: Cubicle): Boolean {
//        return c1.xRange.contains(other.xRange.first) && xRange.contains(other.xRange.last)
//                || yRange.contains(other.yRange.first) && yRange.contains(other.yRange.last)
//                || zRange.contains(other.zRange.first) && zRange.contains(other.zRange.last)
//    }

    fun splitX(cubicle: Cubicle, x: Int): Set<Cubicle> {
        return if (cubicle.first.contains(x)) {
            setOf(
                cubicle.copy(first = cubicle.first.first..(x - 1)),
                cubicle.copy(first = x..x),
                cubicle.copy(first = (x + 1)..cubicle.first.last),
            )
                .filterNot { it.first.isEmpty() }
                .toSet()
        } else {
            setOf(cubicle)
        }
    }

    fun splitY(cubicle: Cubicle, y: Int): Set<Cubicle> {
        return if (cubicle.second.contains(y)) {
            setOf(
                cubicle.copy(second = cubicle.second.first..(y - 1)),
                cubicle.copy(second = y..y),
                cubicle.copy(second = (y + 1)..cubicle.second.last),
            )
                .filterNot { it.second.isEmpty() }
                .toSet()
        } else {
            setOf(cubicle)
        }
    }

    fun splitZ(cubicle: Cubicle, z: Int): Set<Cubicle> {
        return if (cubicle.third.contains(z)) {
            setOf(
                cubicle.copy(third = cubicle.third.first..(z - 1)),
                cubicle.copy(third = z..z),
                cubicle.copy(third = (z + 1)..cubicle.third.last),
            )
                .filterNot { it.third.isEmpty() }
                .toSet()
        } else {
            setOf(cubicle)
        }
    }

    fun fullSplit(cubicle: Cubicle, forCubicle: Cubicle): Set<Cubicle> {
        if (!overLap(cubicle, forCubicle)) {
            return setOf(cubicle)
        }
        return setOf(cubicle)
            .flatMap { splitX(it, forCubicle.first.first) }
            .flatMap { splitX(it, forCubicle.first.last) }
            .flatMap { splitY(it, forCubicle.second.first) }
            .flatMap { splitY(it, forCubicle.second.last) }
            .flatMap { splitZ(it, forCubicle.third.first) }
            .flatMap { splitZ(it, forCubicle.third.last) }
            .toSet()
    }

    private fun overLap(c1: Cubicle, c2: Cubicle): Boolean {
        return (c1.first.contains(c2.first.first) || c1.first.contains(c2.first.last) || c2.first.contains(c1.first.first) || c2.first.contains(c1.first.last))
                && (c1.second.contains(c2.second.first) || c1.second.contains(c2.second.last) || c2.second.contains(c1.second.first) || c2.second.contains(c1.second.last))
                && (c1.third.contains(c2.third.first) || c1.third.contains(c2.third.last) || c2.third.contains(c1.third.first) || c2.third.contains(c1.third.last))
    }

    fun volume(cubicle: Cubicle): Long =
        1L * (cubicle.first.last - cubicle.first.first + 1) *
                (cubicle.second.last - cubicle.second.first + 1) *
                (cubicle.third.last - cubicle.third.first + 1)

    fun contains(c1: Cubicle, c2: Cubicle): Boolean {
        return c1.first.contains(c2.first.first) && c1.first.contains(c2.first.last)
                && c1.second.contains(c2.second.first) && c1.second.contains(c2.second.last)
                && c1.third.contains(c2.third.first) && c1.third.contains(c2.third.last)
    }

}


