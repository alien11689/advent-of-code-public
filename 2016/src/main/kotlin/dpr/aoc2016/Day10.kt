package dpr.aoc2016

import dpr.commons.Util

object Day10 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/10/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val bots = readInput(input)
        while (true) {
            val readyBots = bots.values.filter { it.values.size == 2 && !it.used }
            readyBots.forEach { b ->
                if (b.values.toSet() == setOf(61, 17)) {
                    return b.num.split("_")[1]
                }
                bots[b.num] = b.copy(used = true)
                val (l, u) = b.values.sorted()
                if (!b.lower.startsWith("output")) {
                    val lower = bots[b.lower]!!
                    bots[b.lower] = lower.copy(values = lower.values + l)
                }
                if (!b.upper.startsWith("output")) {
                    val upper = bots[b.upper]!!
                    bots[b.upper] = upper.copy(values = upper.values + u)
                }
            }
        }
    }

    private fun part2(input: List<String>): Any {
        val outputs = mutableMapOf<String, Int>()
        val bots = readInput(input)
        while (true) {
            val readyBots = bots.values.filter { it.values.size == 2 && !it.used }
            if (readyBots.isEmpty()) {
                break
            }
            readyBots.forEach { b ->
                bots[b.num] = b.copy(used = true)
                val (l, u) = b.values.sorted()
                if (!b.lower.startsWith("output")) {
                    val lower = bots[b.lower]!!
                    bots[b.lower] = lower.copy(values = lower.values + l)
                } else {
                    outputs[b.lower] = l
                }
                if (!b.upper.startsWith("output")) {
                    val upper = bots[b.upper]!!
                    bots[b.upper] = upper.copy(values = upper.values + u)
                } else {
                    outputs[b.upper] = u
                }
            }
        }
        return outputs.filter { it.key in setOf("output_0", "output_1", "output_2") }
            .values
            .fold(1) { acc, i -> acc * i }
    }

    private fun readInput(input: List<String>): MutableMap<String, Bot> {
        val bots = mutableMapOf<String, Bot>()
        input.sorted().forEach { line ->
            val parts = line.split(" ")
            if (parts[0] == "bot") {
                val id = listOf(parts[0], parts[1]).joinToString("_")
                bots[id] = Bot(id, listOf(parts[5], parts[6]).joinToString("_"), listOf(parts[10], parts[11]).joinToString("_"))
            } else {
                val id = listOf(parts[4], parts[5]).joinToString("_")
                val dest = bots[id]!!
                bots[id] = dest.copy(values = dest.values + parts[1].toInt())
            }
        }
        return bots
    }

    data class Bot(
        val num: String,
        val lower: String,
        val upper: String,
        val values: List<Int> = listOf(),
        val used: Boolean = false
    )
}
