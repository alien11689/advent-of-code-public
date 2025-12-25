package dpr.aoc2018

import dpr.commons.Util
import java.util.TreeSet

object Day07 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/07/input.txt")
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: List<String>): String {
        val instructions = parseInstructions(input)
        val order = mutableListOf<Char>()

        while (instructions.isNotEmpty()) {
            val next = instructions.filter { it.before.isEmpty() }.minBy { it.dest }
            val letter = next.dest
            order.add(letter)
            instructions.remove(next)
            instructions.filter { it.before.contains(letter) }.forEach {
                it.before.remove(letter)
            }
        }

        return order.joinToString("")
    }

    @JvmStatic
    fun part2(input: List<String>, workers: Int = 5, slowDown: Int = 60): Int {
        val instructions = parseInstructions(input)
        val order = mutableListOf<Char>()

        val workers = (1..workers).map { Worker() }

        val times = ('A'..'Z').associateWith {
            (it.code - 64 + slowDown)
        }

        var ticks = 0
        while (true) {
            workers.filter { it.isFinished() }.map { worker ->
                val letter = worker.cur
                order.add(letter!!)
                worker.makeEmpty()
                val toDelete = instructions.find { it.dest == letter }
                instructions.remove(toDelete)
                instructions.filter { it.before.contains(letter) }.forEach {
                    it.before.remove(letter)
                }
            }
            if (instructions.isEmpty() && workers.all { it.isEmpty() }) {
                break
            }

            val nextWorker = workers.find { it.isEmpty() }
            var shouldTick = true
            if (nextWorker != null && instructions.isNotEmpty()) {
                val available = instructions.filter { it.before.isEmpty() }.filter { instr ->
                    !workers.mapNotNull { it.cur }.contains(instr.dest)
                }.minByOrNull { it.dest }
                if (available != null) {
                    val letter = available.dest
                    nextWorker.assign(letter, times)
                    shouldTick = false
                }
            }
            if (shouldTick) {
                workers.filter { !it.isEmpty() }.forEach { it.tick() }
                ticks++
            }
        }
        return ticks
    }

    private fun parseInstructions(input: List<String>): MutableList<Instr> {
        val instructions = (input.map { it.split(' ')[7] }.toSet())
            .map { Instr(it.first()) }
            .toMutableList()

        input.forEach { l ->
            val split = l.split(' ')
            val dest = split[7].first()
            val from = split[1].first()
            val instr = instructions.find { it.dest == dest }!!
            instr.before.add(from)
        }

        (instructions.flatMap { it.before }.toSet()).forEach { letter ->
            if (instructions.find { it.dest == letter } != null) {
                //nothing
            } else {
                instructions.add(Instr(letter))
            }
        }
        return instructions
    }

    data class Instr(val dest: Char, val before: TreeSet<Char> = TreeSet())

    data class Worker(var cur: Char? = null, var timeout: Int? = null, var seconds: Int = 0) {
        fun isFinished() =
            timeout == seconds

        fun tick() {
            seconds++
        }

        fun isEmpty() =
            cur == null


        fun makeEmpty() {
            cur = null
            seconds = 0
            timeout = null
        }

        fun assign(letter: Char, times: Map<Char, Int>) {
            cur = letter
            timeout = times[letter]
            seconds = 0
        }
    }
}
