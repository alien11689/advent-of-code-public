package pl.touk.dpr.aoc2021

object Day22 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/22/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val instructions = readInstructions(lines)
            .filter { it.isInit() }
        return applyOnEmptySurface(instructions)
    }

    private fun part2(lines: List<String>): Any {
        val instructions = readInstructions(lines)
        return applyOnEmptySurface(instructions)
    }

    private fun applyOnEmptySurface(instructions: List<Instr>): Long {
        var cubicles = setOf<Cubicle>()
        instructions
            .forEachIndexed { idx, instr ->
                if (idx == 0) {
                    cubicles = setOf(instr.cubicle)
                } else {
                    val curCub = instr.cubicle
                    cubicles = cubicles.flatMap { oldCubicle ->
                        oldCubicle.fullSplit(curCub)
                    }.toSet()
                    if (instr.oper == Oper.on) {
                        cubicles = cubicles.filter { !curCub.contains(it) }.toSet() + curCub
                    } else {
                        cubicles = cubicles.filter { !curCub.contains(it) }.toSet()
                    }
                }
//                println("Cubicles size is ${cubicles.size} and volume is ${cubicles.sumOf { volume(it) }}")
            }
        return cubicles.sumOf { it.volume() }
    }

    private fun readInstructions(lines: List<String>) = lines.map { it.split(" ", ",", ".", "=") }
        .map {
            Instr(
                if (it[0] == "on") Oper.on else Oper.off,
                Cubicle(
                    it[2].toInt()..it[4].toInt(),
                    it[6].toInt()..it[8].toInt(),
                    it[10].toInt()..it[12].toInt()
                )
            )
        }

    enum class Oper { on, off }

    data class Instr(val oper: Oper, val cubicle: Cubicle) {
        fun isInit(): Boolean = initCubicle.contains(cubicle)

        companion object {
            val initCubicle = Cubicle(-50..50, -50..50, -50..50)
        }
    }

    data class Cubicle(val xRange: IntRange, val yRange: IntRange, val zRange: IntRange) {
        fun volume(): Long =
            1L * (xRange.last - xRange.first + 1) *
                    (yRange.last - yRange.first + 1) *
                    (zRange.last - zRange.first + 1)

        fun contains(other: Cubicle): Boolean {
            return this.xRange.contains(other.xRange.first) && this.xRange.contains(other.xRange.last)
                    && this.yRange.contains(other.yRange.first) && this.yRange.contains(other.yRange.last)
                    && this.zRange.contains(other.zRange.first) && this.zRange.contains(other.zRange.last)
        }

        fun overLap(other: Cubicle): Boolean {
            return (this.xRange.contains(other.xRange.first) || this.xRange.contains(other.xRange.last) || other.xRange.contains(this.xRange.first) || other.xRange.contains(this.xRange.last))
                    && (this.yRange.contains(other.yRange.first) || this.yRange.contains(other.yRange.last) || other.yRange.contains(this.yRange.first) || other.yRange.contains(this.yRange.last))
                    && (this.zRange.contains(other.zRange.first) || this.zRange.contains(other.zRange.last) || other.zRange.contains(this.zRange.first) || other.zRange.contains(this.zRange.last))
        }

        fun splitX(x: Int): Set<Cubicle> {
            return if (xRange.contains(x)) {
                setOf(
                    copy(xRange = xRange.first..(x - 1)),
                    copy(xRange = x..x),
                    copy(xRange = (x + 1)..xRange.last),
                )
                    .filterNot { it.xRange.isEmpty() }
                    .toSet()
            } else {
                setOf(this)
            }
        }

        fun splitY(y: Int): Set<Cubicle> {
            return if (yRange.contains(y)) {
                setOf(
                    copy(yRange = yRange.first..(y - 1)),
                    copy(yRange = y..y),
                    copy(yRange = (y + 1)..yRange.last),
                )
                    .filterNot { it.yRange.isEmpty() }
                    .toSet()
            } else {
                setOf(this)
            }
        }

        fun splitZ(z: Int): Set<Cubicle> {
            return if (zRange.contains(z)) {
                setOf(
                    copy(zRange = zRange.first..(z - 1)),
                    copy(zRange = z..z),
                    copy(zRange = (z + 1)..zRange.last),
                )
                    .filterNot { it.zRange.isEmpty() }
                    .toSet()
            } else {
                setOf(this)
            }
        }

        fun fullSplit(other: Cubicle): Set<Cubicle> {
            if (!overLap(other)) {
                return setOf(this)
            }
            return setOf(this)
                .flatMap { it.splitX(other.xRange.first) }
                .flatMap { it.splitX(other.xRange.last) }
                .flatMap { it.splitY(other.yRange.first) }
                .flatMap { it.splitY(other.yRange.last) }
                .flatMap { it.splitZ(other.zRange.first) }
                .flatMap { it.splitZ(other.zRange.last) }
                .toSet()
        }
    }

}


