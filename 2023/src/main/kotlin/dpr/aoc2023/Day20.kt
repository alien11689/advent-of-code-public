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

    data class Signal(val from: String, val to: String, val state: Boolean, val iter: Int)

    data class Broadcaster(
        override val name: String,
        override val outputs: List<String>
    ) : Element {
        override fun send(input: Signal) = outputs.map { Signal(name, it, false, input.iter) }
    }

    data class FlipFlop(
        override val name: String,
        override val outputs: List<String>,
        var state: Boolean = false,
    ) : Element {
        override fun send(input: Signal): List<Signal> {
            if (!input.state) {
                state = !state
                return outputs.map { Signal(name, it, state, input.iter) }
            }
            return emptyList()
        }
    }

    data class Conjunction(
        override val name: String,
        override val outputs: List<String>,
        val memory: MutableMap<String, Boolean> = mutableMapOf(),
        val firstChangeToTrue: MutableMap<String, Int> = mutableMapOf()
    ) : Element {
        fun updateMemory(mem: Map<String, Boolean>) {
            memory.putAll(mem)
            firstChangeToTrue.putAll(mem.map { it.key to 0 })
        }

        override fun send(input: Signal): List<Signal> {
            memory[input.from] = input.state
            if (name == "rg" && input.state && firstChangeToTrue[input.from] == 0) {
                firstChangeToTrue[input.from] = input.iter
            }
//            if (name == "rg" && memory.any { it.value }) {
//                println("rg in ${input.iter}: $memory")
//            }
            val allHigh = memory.values.all { it }
            return outputs.map { Signal(name, it, !allHigh, input.iter) }
        }
    }

    private fun part1(lines: List<String>): Any {
        val elements = readElements(lines)
        var resLow = 0L
        var resHigh = 0L
        repeat(1000) {
            val (low, high) = pushTheButton(elements, it + 1)
            resLow += low
            resHigh += high
        }
        return resLow * resHigh
    }

    private fun readElements(lines: List<String>): MutableMap<String, Element> {
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
        return elements
    }

    private fun pushTheButton(elements: MutableMap<String, Element>, iter: Int, part2: Boolean = false): Pair<Long, Long> {
        val queue: Queue<Signal> = LinkedList<Signal>()
        queue.offer(Signal("button", "broadcaster", false, iter))
        var low = 0L
        var high = 0L
        while (queue.isNotEmpty()) {
            val signal = queue.poll()
//            if (signal.to == "rx" && !signal.state) {
//                throw RuntimeException()
//            }
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
        val elements = readElements(lines)
        var i = 1
        while (true) {
            pushTheButton(elements, i, part2 = true)
            // in my input rx receives signal from rg and rg should have all his inputs set to true to emit false to rx
            // By logs I found that it happens on iterations that are prime numbers
            val rg = elements.values.filter { it.name == "rg" }.first() as Conjunction
            if (rg.firstChangeToTrue.values.all { it > 0 }) {
                return rg.firstChangeToTrue.values.fold(1L) { acc, cur -> acc * cur }
            }
            ++i
        }
    }
}

