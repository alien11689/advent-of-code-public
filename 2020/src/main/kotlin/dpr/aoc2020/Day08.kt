package dpr.aoc2020

import dpr.commons.Util

object Day08 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/08/input.txt")
        part1(input)
        part2(input)
    }

    private fun part1(instr: List<String>) {
        val usedInstructions = mutableSetOf<Int>()
        var pointer = 0
        var acc = 0
        while (pointer < instr.size) {
//            println("Instr $pointer, acc $acc")
            if (pointer in usedInstructions) {
                break
            }
            val pair = execute(usedInstructions, pointer, instr, acc)
            acc = pair.first
            pointer = pair.second
        }
        println(acc)
    }

    private fun part2(instr: List<String>) {
        var i = 0
        while (i < instr.size) {
            if (instr[i].contains("nop") || instr[i].contains("jmp")) {
                val newInstr = instr.mapIndexed { idx, inst ->
                    if (i == idx && inst.startsWith("nop")) {
                        inst.replace("nop", "jmp")
                    } else if (i == idx && inst.startsWith("jmp")) {
                        inst.replace("jmp", "nop")
                    } else {
                        inst
                    }
                }
                val (acc, loop) = runProgram(newInstr)
                if (!loop) {
                    println(acc)
                    break
                }
            }
            ++i
        }
    }

    private fun runProgram(instr: List<String>): Pair<Int, Boolean> {
        val usedInstructions = mutableSetOf<Int>()
        var pointer = 0
        var acc = 0
        var infiniteLoop = false
        while (pointer < instr.size) {
            if (pointer in usedInstructions) {
                infiniteLoop = true
                break
            }
            val pair = execute(usedInstructions, pointer, instr, acc)
            acc = pair.first
            pointer = pair.second
        }
        return Pair(acc, infiniteLoop)
    }

    private fun execute(usedInstructions: MutableSet<Int>, pointer: Int, instr: List<String>, acc: Int): Pair<Int, Int> {
        var pointer1 = pointer
        var acc1 = acc
        usedInstructions.add(pointer1)
        val ins = instr[pointer1]
        val split = ins.split(" ")
        if (split[0] == "nop") {
            ++pointer1
        } else if (split[0] == "acc") {
            ++pointer1
            acc1 += split[1].toInt()
        } else {
            pointer1 += split[1].toInt()
        }
        return Pair(acc1, pointer1)
    }
}
