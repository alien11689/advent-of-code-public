package pl.touk.dpr.aoc2021

import java.util.LinkedList
import java.util.Queue

object Day24 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/24/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val instructions = lines.map { Instruction(it.split(' ')) }
        var n = 99999999999999L
        while (n > 10000000000000) {
            val res = runProgram(n.toString(), instructions)
            val z = res['z']!!
            if (z == 0L) {
                return n
            }
//            println("For $n z is $z")
            --n
            while (n.toString().contains('0')) {
                --n
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

    fun iter(w: Long, prevZ: Long, a: Int, b: Int, c: Int): Long {
        var z = prevZ
        var x = z % 26
        z = z / a
        x = x + b
        x = if (x == w) 1 else 0
        x = if (x == 0L) 1 else 0
        var y = 25L
        y = y * x
        y = y + 1
        z = z * y
        y = w
        y = y + 8
        y = y * x
        z = z + y
        return z
    }

    private fun part2(lines: List<String>): Any {

        return -1
    }
}


