package pl.touk.dpr.aoc2020.day08

import pl.touk.dpr.aoc2020.Util

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/08/input.txt")
        part1(input)
        part2(input)
    }

    private fun part1(input: List<String>) {
        val instr = input
        val usedInstructons = mutableSetOf<Int>()
        var pointer = 0
        var acc = 0
        while (pointer < instr.size) {
//            println("Instr $pointer, acc $acc")
            if (pointer in usedInstructons) {
                break
            }
            usedInstructons.add(pointer)
            val ins = instr[pointer]
            val split = ins.split(" ")
            if (split[0] == "nop") {
                ++pointer
            } else if (split[0] == "acc") {
                ++pointer
                acc += split[1].toInt()
            } else {
                pointer += split[1].toInt()
            }
        }
        println(acc)
    }

    private fun part2(input: List<String>) {
        val instr = input
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
        val usedInstructons = mutableSetOf<Int>()
        var pointer = 0
        var acc = 0
        var infiniteLoop = false
        while (pointer < instr.size) {
            if (pointer in usedInstructons) {
                infiniteLoop = true
                break
            }
            usedInstructons.add(pointer)
            val ins = instr[pointer]
            val split = ins.split(" ")
            if (split[0] == "nop") {
                ++pointer
            } else if (split[0] == "acc") {
                ++pointer
                acc += split[1].toInt()
            } else {
                pointer += split[1].toInt()
            }
        }
        return Pair(acc, infiniteLoop)
    }
}