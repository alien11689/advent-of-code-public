package pl.touk.dpr.aoc2019

import pl.touk.dpr.aoc2019.intcode.IntCodeComputer
import pl.touk.dpr.aoc2019.intcode.IntCodeComputer.program
import pl.touk.dpr.aoc2019.intcode.IntCodeComputerState

object Day23 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/23/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val computers = (0L until 50).map {
            val v = IntCodeComputer.parseInput(input)
            val state = IntCodeComputerState(v)
            state.input.offer(it)
            state
        }

        while (true) {
            computers.forEach { state ->
//            println("Running comp ${comp.key}")
                while (true) {
                    if (state.input.isEmpty()) {
                        state.input.offer(-1)
                    }
                    program(state)
                    if (state.output.isEmpty()) {
                        break
                    }
                    while (!state.output.isEmpty()) {
                        val id = state.output.poll().toInt()
                        val x = state.output.poll()
                        val y = state.output.poll()
//                        println("$idx is sending $x $y to $id")
                        if (id == 255) {
                            return y
                        }
                        computers[id].input.offer(x)
                        computers[id].input.offer(y)
                    }
                }
            }
        }

    }

    private fun part2(input: String): Any {
        val computers = (0L until 50).map {
            val v = IntCodeComputer.parseInput(input)
            val state = IntCodeComputerState(v)
            state.input.offer(it)
            state
        }

        var prevNatValue = -2L to -2L

        var natValue = Pair(-1L, -1L)

        while (true) {
            if (computers.all { it.input.isEmpty() && it.output.isEmpty() }) {
                if (natValue.second == prevNatValue.second) {
                    return natValue.second
                }
                prevNatValue = natValue
//                println("Nat is sending $natValue to 0")
                computers[0].input.offer(natValue.first)
                computers[0].input.offer(natValue.second)
            } else {
                computers.forEach { state ->
//                    println("Running comp $idx")
                    while (true) {
                        if (state.input.isEmpty()) {
                            state.input.offer(-1)
                        }
                        program(state)
                        if (state.output.isEmpty()) {
                            break
                        }
                        while (!state.output.isEmpty()) {
                            val id = state.output.poll().toInt()
                            val x = state.output.poll()
                            val y = state.output.poll()
//                            println("$idx is sending $x $y to $id")
                            if (id == 255) {
                                natValue = x to y
                                continue
                            }
                            computers[id].input.offer(x)
                            computers[id].input.offer(y)
                        }
                    }
                }
            }
        }

    }
}
