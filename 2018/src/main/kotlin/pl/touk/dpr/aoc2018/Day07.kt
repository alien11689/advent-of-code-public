package pl.touk.dpr.aoc2018

import java.util.TreeSet

object Day07 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/07/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
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
        val order = mutableListOf<Char>()

        while (!instructions.isEmpty()) {
            val next = instructions.filter { it.before.isEmpty() }.sortedBy { it.dest }.first()
            val letter = next.dest
            order.add(letter)
            instructions.remove(next)
            instructions.filter { it.before.contains(letter) }.forEach {
                it.before.remove(letter)
            }
        }

        return order.joinToString("")
    }

    private fun part2(input: List<String>): Any {
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
        val order = mutableListOf<Char>()

        val workers = (1..5).map { Worker() }

        var ticks = 0
        while (true) {
            workers.filter { it.isFinished() }.map {
                val letter = it.cur
                order.add(letter!!)
                it.makeEmpty()
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
                val available = instructions.filter { it.before.isEmpty() }.filter {
                    !workers.mapNotNull { it.cur }.contains(it.dest)
                }.minByOrNull { it.dest }
                if (available != null) {
                    val letter = available.dest
                    nextWorker.assign(letter)
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

    data class Instr(val dest: Char, val before: TreeSet<Char> = TreeSet())

    data class Worker(var cur: Char? = null, var timeout: Int? = null, var seconds: Int = 0) {
        companion object {
            val times = ('A'..'Z').map {
                it to (it.toInt() - 64 + 60)
            }.toMap()
        }

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

        fun assign(letter: Char) {
            cur = letter
            timeout = times[letter]
            seconds = 0
        }
    }
}