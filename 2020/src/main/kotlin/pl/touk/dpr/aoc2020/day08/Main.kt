package pl.touk.dpr.aoc2020.day08

import pl.touk.dpr.aoc2020.Util

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getFileContent("/08/input.txt")
        part1(input)
        part2(input)
    }

    private fun part1(input: String) {
        val instr = input.lines()
                .filter { it.isNotEmpty() }
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

    private fun part2(input: String) {

    }
}