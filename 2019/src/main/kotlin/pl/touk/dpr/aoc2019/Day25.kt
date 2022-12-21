package pl.touk.dpr.aoc2019

import pl.touk.dpr.aoc2019.intcode.IntCodeComputer
import pl.touk.dpr.aoc2019.intcode.IntCodeComputerState
import java.util.LinkedList

object Day25 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/25/input.txt").trim()
        println(part1(input))
    }

    fun runInteractive(v: MutableMap<Long, Long>) {
        val output = LinkedList<Long>()
        val inputQ = LinkedList<Long>()
        val state = IntCodeComputerState(v, input = inputQ)
        while (true) {
            IntCodeComputer.program(state, output)

            while (output.isNotEmpty()) {
                val cur = output.poll()
                when (cur) {
                    10L -> println()
                    else -> print(cur.toChar())
                }
            }
            var command = readLine()!!
            when (command) {
                "e" -> command = "east"
                "s" -> command = "south"
                "n" -> command = "north"
                "w" -> command = "west"
            }
            IntCodeComputer.instruction(inputQ, command)
        }
    }

    fun solve(v: MutableMap<Long, Long>): String {
        val output = LinkedList<Long>()
        val inputQ = LinkedList<Long>()
        val state = IntCodeComputerState(v, input = inputQ)
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
        IntCodeComputer.program(state, output)
        val out = mutableListOf<String>()
        val line = mutableListOf<Char>()
        val writing = false
        while (output.isNotEmpty()) {
            val cur = output.poll()
            if (writing) {
                when (cur) {
                    10L -> println()
                    else -> print(cur.toChar())
                }
            } else {
                when (cur) {
                    10L -> {
                        out.add(line.joinToString(""))
                        line.clear()
                    }
                    else -> line.add(cur.toChar())
                }
            }
        }
        return out.find { it.contains("\"Oh, hello! You should be able to get in by typing") }
                ?: throw RuntimeException("Turn on writing")
    }

    private fun part1(input: String): String {
        val intCodeInput = IntCodeComputer.parseInput(input)
//        runInteractive(intCodeInput)
        return solve(intCodeInput)
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
