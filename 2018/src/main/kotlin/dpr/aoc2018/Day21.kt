package dpr.aoc2018

import dpr.commons.Util

object Day21 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/21/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val ip = input[0].split(" ").last().toInt()
        val instructions = parseInstructions(input)

        val threshold = 10000
        var curMinR4 = 10000000000L
        var operations = 0
        val registers = mutableListOf(0L, 0L, 0L, 0L, 0L, 0L, 0L)
        val instrToCount = mutableMapOf<Int, Int>()
        while (true) {
            ++operations
            val cur = registers[6].toInt()
//        println("$cur: ${instructions[cur]}")
            registers[ip] = registers[6]
            instrToCount[cur] = (instrToCount[cur] ?: 0) + 1
            instructions[cur].apply(registers)
//        println(registers)
            if (cur == 28 && curMinR4 > registers[4]) {
                curMinR4 = registers[4]
//                println("In iteration ${instrToCount[28]} min is $curMinR4")
                return curMinR4
            }
            if (registers[ip] + 1 >= instructions.size) {
                throw RuntimeException("Finished")
            }
            registers[6] = registers[ip] + 1
            val count = (instrToCount[28] ?: 0) > threshold
            if (count) {
                println("Break is in loop")
                throw RuntimeException("In a loop ")
            }
        }
    }

    private fun part2(input: List<String>): Any {
        val ip = input[0].split(" ").last().toInt()
        val instructions = parseInstructions(input)
        val threshold = 100000
        var operations = 0L
        val result2Iteration = mutableMapOf<Long, Long>()
        val registers = mutableListOf(0L, 0L, 0L, 0L, 0L, 0L, 0L)
        while (true) {
            val cur = registers[6]
            registers[ip] = registers[6]
//        println("$cur: ${instructions[cur.toInt()]}")
//        print("Before $registers || ")
            if (cur == 20L && registers[5] == 0L && registers[2] < registers[3]) {
                while ((registers[5] + 1) * 256 < registers[3]) {
                    operations += 7
                    ++registers[5]
                    registers[2] = (registers[5] + 1) * 256
                }
            }
            instructions[cur.toInt()].apply(registers)
//        println(registers)
            if (cur == 28L) {
                ++operations
                val current = result2Iteration[registers[4]]
                if (current == null) {
                    result2Iteration[registers[4]] = operations
//                    println("Max: ${result2Iteration.maxByOrNull {it.value}!!.key}")
                }
                // 100000 is ok
                // 10000 is too less
                // 25000 is too less
                // 50000 is ok
                if (result2Iteration.size > threshold || operations > threshold * 50000L) {
//                println("Max: ${result2Iteration.maxByOrNull {it.value}!!.key}")
                    return result2Iteration.maxBy { it.value }.key
                }
            }
            if (registers[ip] + 1 >= instructions.size) {
//            println("Halts")
                throw RuntimeException("Halts")
            }
            registers[6] = registers[ip] + 1
        }
    }

    private fun parseInstructions(input: List<String>): List<Operation> {
        return input.drop(1).map { line ->
            val parts = line.split(" ")
            Operation(OperationName.valueOf(parts[0].uppercase()), parts[1].toInt(), parts[2].toInt(), parts[3].toInt())
        }
    }

    enum class OperationName {
        ADDR,
        ADDI,
        SETR,
        SETI,
        MULR,
        MULI,
        BANR,
        BANI,
        BORR,
        BORI,
        GTIR,
        GTRI,
        GTRR,
        EQIR,
        EQRI,
        EQRR,
    }

    data class Operation(val name: OperationName, val a: Int, val b: Int, val c: Int) {
        fun apply(registers: MutableList<Long>) {
            when (name) {
                OperationName.ADDR -> registers[c] = registers[a] + registers[b]
                OperationName.ADDI -> registers[c] = registers[a] + b
                OperationName.SETR -> registers[c] = registers[a]
                OperationName.SETI -> registers[c] = a.toLong()
                OperationName.MULR -> registers[c] = registers[a] * registers[b]
                OperationName.MULI -> registers[c] = registers[a] * b
                OperationName.BANR -> registers[c] = registers[a].and(registers[b])
                OperationName.BANI -> registers[c] = registers[a] and b.toLong()
                OperationName.BORR -> registers[c] = registers[a] or registers[b]
                OperationName.BORI -> registers[c] = registers[a] or b.toLong()
                OperationName.GTIR -> registers[c] = if (a > registers[b]) 1 else 0
                OperationName.GTRI -> registers[c] = if (registers[a] > b) 1 else 0
                OperationName.GTRR -> registers[c] = if (registers[a] > registers[b]) 1 else 0
                OperationName.EQIR -> registers[c] = if (a.toLong() == registers[b]) 1 else 0
                OperationName.EQRI -> registers[c] = if (registers[a] == b.toLong()) 1 else 0
                OperationName.EQRR -> registers[c] = if (registers[a] == registers[b]) 1 else 0
            }
        }
    }
}
