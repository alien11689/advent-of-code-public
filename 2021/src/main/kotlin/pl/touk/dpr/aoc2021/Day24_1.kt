package pl.touk.dpr.aoc2021

import java.util.LinkedList
import java.util.Queue

object Day24_1 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/24/input_three.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val instructions = lines.map { Instruction(it.split(' ')) }
        for (i in 1..9) {
            for (j in 1..9) {
                for (k in 1..9) {
                    val res = runProgram("$i$j$k", instructions)
                    println("For $i$j$k: $res")
                }
            }
        }
        return -1
    }

    private fun runProgram(inp: String, instructions: List<Instruction>): MutableMap<Char, Long> {
        val inputQueue = LinkedList<Int>()
        inp.forEach { inputQueue.offer(it.toString().toInt()) }
        val units = mutableMapOf<Char, Long>('w' to 0, 'x' to 0, 'y' to 0, 'z' to 0)
        val res = instructions.fold(units) { acc, instr ->
            instr.run(acc, inputQueue)
        }
        return res
    }

    data class Instruction(val name: String, val op1: Char, val op2AsLong: Long?, val op2AsUnit: Char?) {
        constructor(parts: List<String>) : this(
            parts[0], parts[1].first(),
            if (parts.size > 2 && parts[2] !in setOf("w", "x", "y", "z")) parts[2].toLong() else null,
            if (parts.size > 2 && parts[2] in setOf("w", "x", "y", "z")) parts[2].first() else null
        )

        fun run(map: MutableMap<Char, Long>, inputQueue: Queue<Int>): MutableMap<Char, Long> {
            when (name) {
                "inp" -> map[op1] = inputQueue.poll().toLong()
                "add" -> map[op1] = map[op1]!! + (if (op2AsUnit != null) map[op2AsUnit]!! else op2AsLong!!)
                "mul" -> map[op1] = map[op1]!! * (if (op2AsUnit != null) map[op2AsUnit]!! else op2AsLong!!)
                "div" -> map[op1] = map[op1]!! / (if (op2AsUnit != null) map[op2AsUnit]!! else op2AsLong!!)
                "mod" -> map[op1] = map[op1]!! % (if (op2AsUnit != null) map[op2AsUnit]!! else op2AsLong!!)
                "eql" -> map[op1] = if (map[op1]!! == (if (op2AsUnit != null) map[op2AsUnit]!! else op2AsLong!!)) 1 else 0
                "set" -> map[op1] = (if (op2AsUnit != null) map[op2AsUnit]!! else op2AsLong!!)
            }
            return map
        }
    }

    private fun part2(lines: List<String>): Any {

        return -1
    }
}


