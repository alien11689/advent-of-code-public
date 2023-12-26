package dpr.aoc2015

import dpr.commons.Util
import kotlin.math.max

object Day06 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/06/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val lights = buildLights(false)
        input.forEach { instr ->
            val parts = instr.split(Regex("[ ,]+"))
            if (parts[0].startsWith("turn")) {
                val x1 = parts[2].toInt()
                val y1 = parts[3].toInt()
                val x2 = parts[5].toInt()
                val y2 = parts[6].toInt()
                (x1..x2).forEach { x ->
                    (y1..y2).forEach { y ->
                        lights[x][y] = parts[1] == "on"
                    }
                }
            } else {
                val x1 = parts[1].toInt()
                val y1 = parts[2].toInt()
                val x2 = parts[4].toInt()
                val y2 = parts[5].toInt()
                (x1..x2).forEach { x ->
                    (y1..y2).forEach { y ->
                        lights[x][y] = !lights[x][y]
                    }
                }
            }
        }
        return lights.flatten().count { it }
    }

    private inline fun <reified T> buildLights(defaultValue: T): MutableList<MutableList<T>> {
        val lights = mutableListOf<MutableList<T>>()
        (0..999).forEach { _ ->
            val l = mutableListOf<T>()
            (0..999).forEach { _ ->
                l.add(defaultValue)
            }
            lights.add(l)
        }
        return lights
    }

    private fun part2(input: List<String>): Any {
        val lights = buildLights(0)
        input.forEach { instr ->
            val parts = instr.split(Regex("[ ,]+"))
            if (parts[0].startsWith("turn")) {
                val x1 = parts[2].toInt()
                val y1 = parts[3].toInt()
                val x2 = parts[5].toInt()
                val y2 = parts[6].toInt()
                val on = parts[1] == "on"
                (x1..x2).forEach { x ->
                    (y1..y2).forEach { y ->
                        lights[x][y] = max(0, lights[x][y] + if (on) 1 else -1)
                    }
                }
            } else {
                val x1 = parts[1].toInt()
                val y1 = parts[2].toInt()
                val x2 = parts[4].toInt()
                val y2 = parts[5].toInt()
                (x1..x2).forEach { x ->
                    (y1..y2).forEach { y ->
                        lights[x][y] += 2
                    }
                }
            }
        }
        return lights.flatten().sum()
    }
}
