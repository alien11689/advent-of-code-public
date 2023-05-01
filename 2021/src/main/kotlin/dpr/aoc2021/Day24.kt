package dpr.aoc2021

import java.util.LinkedList
//import java.util.Queue
import java.util.Stack

object Day24 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        println(part1())
        println(part2())
    }

    private fun part1(): Any {
//        val instructions = lines.map { Instruction(it.split(' ')) }
        val res = (1..14).map { 0 }.toMutableList()
        val usedInstructions = mutableSetOf<Int>()
        val digitPrecedence = 9 downTo 1
        return process(usedInstructions, res, digitPrecedence)
//        val inp = "29998199999999"
//        val inp = "5979" // two first and two last are ok
//        val inp = "59        7979" // two first and four last are ok
//        val inp = "59     2699979" // two first and four last are ok
//        val inp = "599  426997979" // two first and four last are ok
//        val inp = "599  426997979" // two first and four last are ok
//        val inp = "59998426997979" // two first and four last are ok

//        for (i in 9 downTo 1) {
//            for (j in 9 downTo 1) {
//                val inp = "599${i}  ${j}426997979"
//                if (iterProgram(inp) == 0L) {
//                    println("match for $i $j")
//                }
//            }
//        }
//        val program = runProgram(inp, instructions)
//        val iterRes = iterProgram(inp)
//        println("program ${program['z']} and iterRes $iterRes")
//        return -1
    }

    private fun findMatching(res: MutableList<Int>, p: Pair<Int, Int>, instructionList: List<List<Int>>, digitPrecedence: IntProgression) {
        for (i in digitPrecedence) {
            for (j in digitPrecedence) {
                res[p.first] = i
                res[p.second] = j
                val inp = res.filter { it > 0 }.map { it.toLong() }.joinToString("")
                if (iterProgram(inp, instructionList) == 0L) {
                    return
                }
            }
        }
        return
    }

    private fun iterProgram(inp: String, instr: List<List<Int>>): Long {
        val inputQueue = LinkedList<Int>()
        inp.filter { it.isDigit() }.forEach { inputQueue.offer(it.toString().toInt()) }
//        println("z[0] = 0")
        return instr.fold(0L) { prevZ, cur ->
            val z = iter(inputQueue.poll().toLong(), prevZ, cur[0], cur[1], cur[2])
//            println("z[${14 - inputQueue.size}] = $z (after negating = ${cur[0] == 26})")
            z
        }
    }

//    fun iterProgram2(): Long {
//        // this function shows relation between digits when printing once
//        val inp = "59999999999999".map { it.toString().toLong() }.toMutableList()
//        val inputQueue = LinkedList<Int>()
//        inp.forEach { inputQueue.offer(it.toString().toInt()) }
//        println("z[0] = 0")
//        return vars.fold(0L) { prevZ, cur ->
//            val z = iter(inputQueue.poll().toLong(), prevZ, cur[0], cur[1], cur[2])
//            println("z[${14 - inputQueue.size}] = $z (after negating = ${cur[0] == 26})")
//            z
//        }
//    }

//    private fun runProgram(inp: String, instructions: List<Instruction>): MutableMap<Char, Long> {
//        val inputQueue = LinkedList<Int>()
//        inp.forEach { inputQueue.offer(it.toString().toInt()) }
//        val units = mutableMapOf<Char, Long>('w' to 0, 'x' to 0, 'y' to 0, 'z' to 0)
//        val res = instructions.fold(units) { acc, instr ->
//            instr.run(acc, inputQueue)
//        }
//        return res
//    }

//    data class Instruction(val name: String, val op1: Char, val op2AsLong: Long?, val op2AsUnit: Char?) {
//        constructor(parts: List<String>) : this(
//            parts[0], parts[1].first(),
//            if (parts.size > 2 && parts[2] !in setOf("w", "x", "y", "z")) parts[2].toLong() else null,
//            if (parts.size > 2 && parts[2] in setOf("w", "x", "y", "z")) parts[2].first() else null
//        )
//
//        fun run(map: MutableMap<Char, Long>, inputQueue: Queue<Int>): MutableMap<Char, Long> {
//            when (name) {
//                "inp" -> map[op1] = inputQueue.poll().toLong()
//                "add" -> map[op1] = map[op1]!! + (if (op2AsUnit != null) map[op2AsUnit]!! else op2AsLong!!)
//                "mul" -> map[op1] = map[op1]!! * (if (op2AsUnit != null) map[op2AsUnit]!! else op2AsLong!!)
//                "div" -> map[op1] = map[op1]!! / (if (op2AsUnit != null) map[op2AsUnit]!! else op2AsLong!!)
//                "mod" -> map[op1] = map[op1]!! % (if (op2AsUnit != null) map[op2AsUnit]!! else op2AsLong!!)
//                "eql" -> map[op1] = if (map[op1]!! == (if (op2AsUnit != null) map[op2AsUnit]!! else op2AsLong!!)) 1 else 0
//                "set" -> map[op1] = (if (op2AsUnit != null) map[op2AsUnit]!! else op2AsLong!!)
//            }
//            return map
//        }
//    }

    private fun iter(w: Long, prevZ: Long, a: Int, b: Int, c: Int): Long {
        val x = if (prevZ % 26 + b != w) 1 else 0
        return (prevZ / a) * (25L * x + 1) + (w + c) * x
    }

    private val vars = listOf(
            listOf(1, 13, 8),
            listOf(1, 12, 13),
            listOf(1, 12, 8),
            listOf(1, 10, 10),
            listOf(26, -11, 12),
            listOf(26, -13, 1),
            listOf(1, 15, 13),
            listOf(1, 10, 5),
            listOf(26, -2, 10),
            listOf(26, -6, 3),
            listOf(1, 14, 2),
            listOf(26, 0, 2),
            listOf(26, -15, 12),
            listOf(26, -4, 7),
    )

    private fun buildStackPairs(instructions: List<List<Int>>): List<Pair<Int, Int>> {
        val res = mutableListOf<Pair<Int, Int>>()
        val s = Stack<Int>()
        instructions.forEachIndexed { idx, instr ->
            if (instr[0] == 1) {
                s.push(idx)
            } else {
                val matching = s.pop()
                res.add(matching to idx)
            }
        }
        return res.reversed()
    }

    private fun part2(): Any {
        val res = (1..14).map { 0 }.toMutableList()
        val usedInstructions = mutableSetOf<Int>()
        val digitPrecedence = 1..9
        return process(usedInstructions, res, digitPrecedence)
    }

    private fun process(usedInstructions: MutableSet<Int>, res: MutableList<Int>, digitPrecedence: IntProgression): String {
        val stackPars = buildStackPairs(vars)
        stackPars.forEach { p ->
            usedInstructions.addAll(p.toList())
            val instructionList = vars.filterIndexed { index, _ -> index in usedInstructions }
            findMatching(res, p, instructionList, digitPrecedence)
        }
        return res.joinToString("")
    }

//    fun fullIter(w: Long, prevZ: Long, a: Int, b: Int): Long {
//        var z = prevZ
//        var x = z % 26
//        z = z / a
//        x = x + b
//        x = if (x == w) 1 else 0
//        x = if (x == 0L) 1 else 0
//        var y = 25L
//        y = y * x
//        y = y + 1
//        z = z * y
//        y = w
//        y = y + 8
//        y = y * x
//        z = z + y
//        return z
//    }


}


