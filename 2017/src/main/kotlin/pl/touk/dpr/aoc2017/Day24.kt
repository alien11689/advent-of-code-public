package pl.touk.dpr.aoc2017

import java.util.LinkedList

object Day24 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/24/input.txt")
        val chains = chains(input)
        println(part1(chains))
        println(part2(chains))
    }

    private fun part1(chains: Set<Chain>) = chains.maxOf { it.strength() }

    private fun part2(chains: Set<Chain>): Int {
        val maxLength = chains.maxOf { it.cpus.size }
        return chains.filter { it.cpus.size == maxLength }.maxOf { it.strength() }
    }

    private fun chains(lines: List<String>): Set<Chain> {
        val cpus = lines.map { line ->
            Cpu(line.split('/').map { it.toInt() })
        }

        val chains = mutableSetOf<Chain>()
        val queue = LinkedList<Chain>()
        queue.offer(Chain(setOf(), 0))

        while (queue.isNotEmpty()) {
            val current = queue.poll()
            cpus.mapNotNull {
                current.connectTo(it)
            }.forEach {
                queue.offer(it)
                chains.add(it)
            }
        }

        return chains
    }

    data class Cpu(val pins: List<Int>)

    data class Chain(val cpus: Set<Cpu>, val lastPin: Int) {
        fun connectTo(c: Cpu): Chain? {
            if (c in cpus) {
                return null
            }
            if (lastPin !in c.pins) {
                return null
            }
            return Chain(cpus + c, if (lastPin == c.pins[0]) c.pins[1] else c.pins[0])
        }

        fun strength(): Int = cpus.flatMap { it.pins }.sum()
    }
}
