package dpr.aoc2017

import dpr.commons.Util
import kotlin.math.absoluteValue

object Day20 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/20/input.txt")
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: List<String>): Int {
        val particles = readInput(input)

        repeat(1000) {
            particles.forEach {
                it.updateV()
                it.updateP()
            }
        }
        return particles.minByOrNull { it.from0() }!!.id
    }

    private fun readInput(input: List<String>): List<Particle> {
        return input.mapIndexed { index, line ->
            val parts = line.split(Regex("[<>]"))
            Particle(
                index,
                p = Coord.from(parts[1].trim()),
                v = Coord.from(parts[3].trim()),
                a = Coord.from(parts[5].trim())
            )
        }
    }

    @JvmStatic
    fun part2(input: List<String>): Int {
        var particles = readInput(input)
        repeat(1000) {
            val hits = mutableMapOf<Coord, Int>()
            particles.forEach {
                it.updateV()
                it.updateP()
                hits[it.p] = 1 + (hits[it.p] ?: 0)
            }
            val doubles = hits.filter { it.value > 1 }.keys
            particles = particles.filter { it.p !in doubles }
        }
        return particles.size
    }

    data class Coord(val x: Int, val y: Int, val z: Int) {
        companion object {
            fun from(str: String): Coord {
                val parts = str.split(",")
                return Coord(parts[0].toInt(), parts[1].toInt(), parts[2].toInt())
            }
        }
    }

    data class Particle(val id: Int, var p: Coord, var v: Coord, val a: Coord) {
        fun updateV() {
            v = Coord(
                x = v.x + a.x,
                y = v.y + a.y,
                z = v.z + a.z,
            )
        }

        fun updateP() {
            p = Coord(
                x = v.x + p.x,
                y = v.y + p.y,
                z = v.z + p.z,
            )
        }

        fun from0(): Int {
            return p.x.absoluteValue + p.y.absoluteValue + p.z.absoluteValue
        }
    }
}
