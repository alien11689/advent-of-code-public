package pl.touk.dpr.aoc2021

object Day22 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/22/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val init = setOf<Triple<Int, Int, Int>>()
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
            .fold(init) { acc, instr ->
                val newS = if (instr.oper == Oper.off) {
                    acc.filterNot { it.first in instr.xRange && it.second in instr.yRange && it.third in instr.zRange }.toSet()
                } else {
                    acc + instr.generateTriples()
                }
//                println(newS)
                println("On is ${newS.size} on ${++step}")

                newS
            }
        return cubes.size
    }

    private fun part2(lines: List<String>): Any {

        return -1
    }

    enum class Oper { on, off }

    data class Instr(val oper: Oper, val xRange: IntRange, val yRange: IntRange, val zRange: IntRange) {
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
    }
}


