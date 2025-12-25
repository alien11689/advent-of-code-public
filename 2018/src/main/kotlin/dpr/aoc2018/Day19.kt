package dpr.aoc2018

import dpr.commons.Util

object Day19 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/19/input.txt")
        println(part1(input))
        println(part2())
    }

    @JvmStatic
    fun part1(lines: List<String>): Int {
        val ip = lines[0].split(" ").last().toInt()
        val registers = mutableListOf(0, 0, 0, 0, 0, 0, 0)
        val instructions = lines.drop(1).map { line ->
            val parts = line.split(" ")
            Operation(parts[0], parts[1].toInt(), parts[2].toInt(), parts[3].toInt())
        }
        while (true) {
            val cur = registers[6]
            registers[ip] = registers[6]
//            println(instructions[cur])
//            print("Before $registers || ")
            instructions[cur].apply(registers)
//            println(registers)
            if (registers[ip] + 1 >= instructions.size) {
                break
            }
            registers[6] = registers[ip] + 1
        }
        return registers[0]
    }

    @JvmStatic
    fun part2(): Long {
//        println("After optimizations and observations:")
        return (1..10551314).sumOf { if (10551314 % it == 0) 10551314L / it else 0L }
    }

    data class Operation(val name: String, val a: Int, val b: Int, val c: Int) {
        fun apply(registers: MutableList<Int>) {
            when (name) {
                "addr" -> registers[c] = registers[a] + registers[b]
                "addi" -> registers[c] = registers[a] + b
                "setr" -> registers[c] = registers[a]
                "seti" -> registers[c] = a
                "mulr" -> registers[c] = registers[a] * registers[b]

                "muli" -> registers[c] = registers[a] * b
                "banr" -> registers[c] = registers[a].and(registers[b])

                "bani" -> registers[c] = registers[a] and b
                "borr" -> registers[c] = registers[a] or registers[b]
                "bori" -> registers[c] = registers[a] or b

                "gtir" -> registers[c] = if (a > registers[b]) 1 else 0

                "gtri" -> registers[c] = if (registers[a] > b) 1 else 0

                "gtrr" -> registers[c] = if (registers[a] > registers[b]) 1 else 0

                "eqir" -> registers[c] = if (a == registers[b]) 1 else 0

                "eqri" -> registers[c] = if (registers[a] == b) 1 else 0

                "eqrr" -> registers[c] = if (registers[a] == registers[b]) 1 else 0

            }
        }
    }
}
