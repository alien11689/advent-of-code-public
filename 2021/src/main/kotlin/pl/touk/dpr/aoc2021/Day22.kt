package pl.touk.dpr.aoc2021

typealias Cubicle = Triple<IntRange, IntRange, IntRange>

object Day22 {

    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/22/input2.txt")
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
            .filter { it.isInit() }
            .forEachIndexed { idx, instr ->
                if (idx == 0) {
                    cubicles = setOf(instr.cubicle())
                } else {
                    val curCub = instr.cubicle()
                    val curAsSubs = cubicles.flatMap { fullSplit(curCub, it) }
                    curAsSubs.forEach { c1 ->
                        curAsSubs.forEach { c2 ->
                            if (c1 != c2) {
                                if (overLap(c1, c2)) {
                                    println("For new one $c1 overlap $c2")
                                }
                            }
                        }
                    }
//                    println("Intersecting $cubicles by $curCub")
                    println(curAsSubs)
                    cubicles = cubicles.flatMap { oldCubicle ->
                        fullSplit(oldCubicle, curCub)
                    }.toSet()
//                    println("Cubicles before operation $cubicles")
                    if (instr.oper == Oper.on) {
//                        print("Cubicles now is ${cubicles.size} and adding ${curAsSubs.size} ")
                        cubicles = cubicles + curAsSubs
//                        println("results in size ${cubicles.size}")
                    } else {
                        cubicles = cubicles - curAsSubs
                    }
//                    println("Result in $cubicles")
                }
                cubicles.forEach { c1 ->
                    cubicles.forEach { c2 ->
                        if (c1 != c2) {
                            if (overLap(c1, c2)) {
                                println("$c1 overlap $c2")
                            }
                        }
                    }
                }
                println("Cubicles size is ${cubicles.size} and volume is ${cubicles.sumOf { volume(it) }}")
                if (idx > 0) {
                    return -1
                }
            }

        // all instructions overlap
//        val s = mutableSetOf<Pair<Instr, Instr>>()
//        for (i1 in instructions) {
//            for (i2 in instructions) {
//                if (i1 != i2) {
//                    if (i1.overlap(i2) || i2.overlap(i1)) {
//                        println("$i1 overlap $i2")
//                        s.add(i1 to i2)
//                    }
//                }
//            }
//        }
//        val allOverllapping = s.flatMap { setOf(it.first, it.second) }.toSet()
//        for (i1 in instructions) {
//            if (i1 !in allOverllapping) {
//                println("   $i1 is alone")
//            }
//        }
//        for(i in instructions){
//            println("${i.size()} is size of $i")
//        }

//        cubicles.forEach {
//            println("volume of $it is ${volume(it)}")
//        }
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

}


