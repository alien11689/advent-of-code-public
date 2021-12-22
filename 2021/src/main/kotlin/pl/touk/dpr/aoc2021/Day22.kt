package pl.touk.dpr.aoc2021

object Day22 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/22/input3.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val initPoints = setOf<Triple<Int, Int, Int>>()
        var step = 1
        val cubes = lines.map { it.split(" ", ",", ".", "=") }
            .map {
                Instr(
                    if (it[0] == "on") Oper.on else Oper.off,
                    it[2].toInt()..it[4].toInt(),
                    it[6].toInt()..it[8].toInt(),
                    it[10].toInt()..it[12].toInt(),
                )
            }
            .filter { it.isInit() }
            .fold(initPoints) { acc, instr ->
                val newS = if (instr.oper == Oper.off) {
                    acc.filterNot { it.first in instr.xRange && it.second in instr.yRange && it.third in instr.zRange }.toSet()
                } else {
                    acc + instr.generateTriples()
                }
//                println(newS)
//                println("On is ${newS.size} on ${++step}")

                newS
            }
        return cubes.size
    }

    private fun part2(lines: List<String>): Any {
        val initPoints = setOf<Triple<Int, Int, Int>>()
        var step = 1
        val instructions = lines.map { it.split(" ", ",", ".", "=") }
            .mapIndexed { idx, it ->
                Instr(
                    if (it[0] == "on") Oper.on else Oper.off,
                    it[2].toInt()..it[4].toInt(),
                    it[6].toInt()..it[8].toInt(),
                    it[10].toInt()..it[12].toInt(),
                    idx
                )
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

        return -1
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

        fun overlap(other: Instr): Boolean {
            return xRange.contains(other.xRange.first) && xRange.contains(other.xRange.last)
                    || yRange.contains(other.yRange.first) && yRange.contains(other.yRange.last)
                    || zRange.contains(other.zRange.first) && zRange.contains(other.zRange.last)
        }
    }
}


