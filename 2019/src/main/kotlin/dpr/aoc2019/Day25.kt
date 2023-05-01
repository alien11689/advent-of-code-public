package dpr.aoc2019

import dpr.aoc2019.intcode.IntCodeComputer
import dpr.aoc2019.intcode.IntCodeComputerState

object Day25 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/25/input.txt").trim()
        println(part1(input))
    }

    fun runInteractive(input: String) {
        val state = IntCodeComputerState.init(input)
        val output = state.output
        val inputQ = state.input
        while (true) {
            IntCodeComputer.program(state)

            while (output.isNotEmpty()) {
                when (val cur = output.poll()) {
                    10L -> println()
                    else -> print(cur.toInt().toChar())
                }
            }
            var command = readln()
            when (command) {
                "e" -> command = "east"
                "s" -> command = "south"
                "n" -> command = "north"
                "w" -> command = "west"
            }
            IntCodeComputer.instruction(inputQ, command)
        }
    }

    fun solve(input: String): String {
        val state = IntCodeComputerState.init(input)
        val output = state.output
        val inputQ = state.input
        listOf(
                "south",
                "west",
                "take hologram",
                "south",
                "west",
                "west",
                "take hypercube",
                "east",
                "east",
                "north",
                "east",
                "south",
                "west",
                "north",
                "take coin",
                "south",
                "east",
                "take cake",
                "east",
                "south",
                "east",
                "south",
                "south",
        ).forEach {
            IntCodeComputer.instruction(inputQ, it)
        }
        IntCodeComputer.program(state)
        val out = mutableListOf<String>()
        val line = mutableListOf<Char>()
        val writing = false
        while (output.isNotEmpty()) {
            val cur = output.poll()
            if (writing) {
                when (cur) {
                    10L -> println()
                    else -> print(cur.toInt().toChar())
                }
            } else {
                when (cur) {
                    10L -> {
                        out.add(line.joinToString(""))
                        line.clear()
                    }

                    else -> line.add(cur.toInt().toChar())
                }
            }
        }
        return out.find { it.contains("\"Oh, hello! You should be able to get in by typing") }
                ?: throw RuntimeException("Turn on writing")
    }

    private fun part1(input: String): String {
//        runInteractive(input)
        val solution = solve(input)
        return solution.split(" ")[11]
    }
    //photons > X
//molten lava > X
//food ration > X
//astrolabe > X
//space law space brochure > X
//
//cake < X
//escape pod < X
//hypercube < X
//hologram < X
//wreath < X
//coin < X
//
//hologram + cake + wreath + coin < X
//hologram + cake + wreath + coin + hypercube > X
//coin < hypercube
//
    //Solution:
//s
//w
//take hologram
//s
//w
//w
//take hypercube
//e
//e
//n
//e
//s
//w
//n
//take coin
//s
//e
//take cake
//e
//s
//e
//s
//s
}
