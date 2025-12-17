package dpr.aoc2015

import dpr.commons.Util

object Day23 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/23/input.txt")
        val instr = readInput(input)
        println(part1(instr))
        println(part2(instr))
    }

    @JvmStatic
    fun part1(instr: List<List<String>>, register: String = "b"): Int {
        val reg = mutableMapOf(Pair("a", 0), Pair("b", 0))
        runProgram(instr, reg)
        return reg[register]!!
    }

    @JvmStatic
    fun readInput(input: List<String>) = input.map { it.split(Regex("[ ,]+")).toList() }

    private fun runProgram(instr: List<List<String>>, reg: MutableMap<String, Int>) {
        var i = 0
        while (i < instr.size) {
            val cur = instr[i]
            when (cur[0]) {
                "inc" -> {
                    reg[cur[1]] = reg[cur[1]]!! + 1
                    ++i
                }

                "tpl" -> {
                    reg[cur[1]] = reg[cur[1]]!! * 3
                    ++i
                }

                "hlf" -> {
                    reg[cur[1]] = reg[cur[1]]!! / 2
                    ++i
                }

                "jie" -> if (reg[cur[1]]!! % 2 == 0) i += cur[2].toInt() else i++
                "jio" -> if (reg[cur[1]]!! == 1) i += cur[2].toInt() else i++
                "jmp" -> i += cur[1].toInt()
                else -> throw RuntimeException(cur[0])
            }
        }
    }

    @JvmStatic
    fun part2(instr: List<List<String>>): Int {
        val reg = mutableMapOf(Pair("a", 1), Pair("b", 0))
        runProgram(instr, reg)
        return reg["b"]!!
    }
}
