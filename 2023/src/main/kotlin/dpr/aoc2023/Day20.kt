package dpr.aoc2023

import dpr.commons.Util
import java.util.LinkedList
import java.util.Queue

object Day20 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/20/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/20/test1.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/20/test2.txt")
        println(part1(lines))
        println(part2(lines))
    }

    sealed interface Element {
        val name: String
        val outputs: List<String>

        fun send(input: Signal): List<Signal>
    }

    data class Signal(val from: String, val to: String, val state: Boolean)

    data class Broadcaster(
        override val name: String,
        override val outputs: List<String>
    ) : Element {
        override fun send(signal: Signal) = outputs.map { Signal(name, it, false) }
    }

    data class FlipFlop(
        override val name: String,
        override val outputs: List<String>,
        var state: Boolean = false,
    ) : Element {
        override fun send(input: Signal): List<Signal> {
            if (!input.state) {
                state = !state
                return outputs.map { Signal(name, it, state) }
            }
            return emptyList()
        }
    }

    data class Conjunction(
        override val name: String,
        override val outputs: List<String>,
        val memory: MutableMap<String, Boolean> = mutableMapOf(),
    ) : Element {
        fun updateMemory(mem: Map<String, Boolean>) {
            memory.putAll(mem)
        }

        override fun send(input: Signal): List<Signal> {
            memory[input.from] = input.state
            val allHigh = memory.values.all { it }
            return outputs.map { Signal(name, it, !allHigh) }
        }
    }

    private fun part1(lines: List<String>): Any {
        val elements = mutableMapOf<String, Element>()
        lines.forEach { line ->
            val split = line.split(Regex("[->, ]+"))
            val elementName = split[0].drop(1)
            val elementType = split[0][0]
            val outputs = split.drop(1)
            val element = when (elementType) {
                '%' -> FlipFlop(elementName, outputs)
                '&' -> Conjunction(elementName, outputs)
                else -> Broadcaster(split[0], outputs)
            }
            elements[element.name] = element
        }
        elements.values.filterIsInstance<Conjunction>().forEach { conjuntion ->
            conjuntion.updateMemory(elements.values.filter { conjuntion.name in it.outputs }.associate { it.name to false })
        }
//        elements.forEach { println(it) }

        var resLow = 0L
        var resHigh = 0L
        repeat(1000) {
            val (low, high) = pushTheButton(elements)
            resLow += low
            resHigh += high
        }
        return resLow * resHigh
    }

    private fun pushTheButton(elements: MutableMap<String, Element>): Pair<Long, Long> {
        val queue: Queue<Signal> = LinkedList<Signal>()
        queue.offer(Signal("button", "broadcaster", false))
        var low = 0L
        var high = 0L
        while (queue.isNotEmpty()) {
            val signal = queue.poll()
//            println("Processing $signal")
            if (signal.state) {
                ++high
            } else {
                ++low
            }
            elements[signal.to]?.send(signal)?.forEach { queue.offer(it) }
        }
        return low to high
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

