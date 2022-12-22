package pl.touk.dpr.aoc2017

import java.util.LinkedList

object Day24 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/24/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(lines: List<String>): Any {
        val cpus = lines.map { line ->
            Cpu(line.split('/').map { it.toInt() })
        }

        var bestLength = 0
        val queue = LinkedList<Chain>()
        queue.offer(Chain(listOf(), 0))

        while (queue.isNotEmpty()) {
            val parent = queue.poll()
            if (parent.length > bestLength) {
                bestLength = parent.length
            }
            cpus.mapNotNull {
                parent.connectTo(it)
            }.forEach {
                queue.offer(it)
            }
        }

        return bestLength
    }

    private fun part2(lines: List<String>): Any {
        val cpus = lines.map { line ->
            Cpu(line.split('/').map { it.toInt() })
        }
        val l2sizes = mutableMapOf<Int, List<Int>>()
        val queue = LinkedList<Chain>()
        queue.offer(Chain(listOf(), 0))

        while (queue.isNotEmpty()) {
            val parent = queue.poll()
            l2sizes[parent.length2] = (l2sizes[parent.length2] ?: listOf()) + parent.length
            cpus.mapNotNull {
                parent.connectTo(it)
            }.forEach {
                queue.offer(it)
            }
        }

        val maxLength = l2sizes.keys.maxOrNull()!!
        return l2sizes[maxLength]!!.maxOrNull()!!
    }

    data class Cpu(val pins: List<Int>)

    data class Chain(val cpus: List<Cpu>, val lastPin: Int, val length: Int = cpus.flatMap { it.pins }.sum(), val length2: Int = cpus.size) {
        fun connectTo(c: Cpu): Chain? {
            if (c in cpus) {
                return null
            }
            if (lastPin !in c.pins) {
                return null
            }
            return Chain(cpus + c, if (lastPin == c.pins[0]) c.pins[1] else c.pins[0])
        }
    }
}
