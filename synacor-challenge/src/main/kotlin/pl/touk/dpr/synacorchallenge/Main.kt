package pl.touk.dpr.synacorchallenge

import java.util.Stack

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
        val bytes = javaClass.getResource("/challenge.bin").readBytes()
        val program = readProgram(bytes)
        runProgram(program)
    }

    private fun readProgram(bytes: ByteArray): MutableList<Int> {
        val program = mutableListOf<Int>()
        var i = 0
        while (i < bytes.size) {
            // have to read bytes as unsigned integer, bytes are by default signed
            val a = bytes[i].toInt().and(0xff)
            val b = bytes[i + 1].toInt().and(0xff)
            // little endian
            val code = a + b * Math.pow(2.toDouble(), 8.toDouble()).toInt()
            program.add(code)
            i += 2
        }
        return program
    }

    private fun valueOrRegister(v: Int, registry: Map<Int, Int>): Int =
            if (v >= BASE) registry[v % BASE]!! else v

    private fun runProgram(program: MutableList<Int>) {
        val stack = Stack<Int>()
        val registers = mutableMapOf<Int, Int>()
        (0..7).forEach { registers[it] = 0 }
        var instrPointer = 0
        var debug = false
        while (instrPointer < program.size) {
            val opcode = program[instrPointer]
            when (opcode) {
                Opcodes.NOOP -> {
                    instrPointer += 1
                }
                Opcodes.OUT -> {
                    print(valueOrRegister(program[instrPointer + 1], registers).toChar())
                    instrPointer += 2
                }
                Opcodes.JMP -> {
                    if (debug) System.err.println("JMP ${program[instrPointer + 1]} $registers")
                    instrPointer = valueOrRegister(program[instrPointer + 1], registers)
                }
                Opcodes.JT -> {
                    if (debug) System.err.println("JT ${program[instrPointer + 1]} ${program[instrPointer + 2]} $registers")
                    if (valueOrRegister(program[instrPointer + 1], registers) != 0) {
                        instrPointer = valueOrRegister(program[instrPointer + 2], registers)
                    } else {
                        instrPointer += 3
                    }
                }
                Opcodes.JF -> {
                    if (debug) System.err.println("JF ${program[instrPointer + 1]} ${program[instrPointer + 2]}} $registers")
                    if (valueOrRegister(program[instrPointer + 1], registers) == 0) {
                        instrPointer = valueOrRegister(program[instrPointer + 2], registers)
                    } else {
                        instrPointer += 3
                    }
                }
                Opcodes.SET -> {
                    if (debug) System.err.println("SET ${program[instrPointer + 1]} ${program[instrPointer + 2]}} $registers")
                    registers[program[instrPointer + 1] % BASE] = valueOrRegister(program[instrPointer + 2], registers)
                    instrPointer += 3
                }
                Opcodes.ADD -> {
                    if (debug) System.err.println("ADD ${program[instrPointer + 1]} ${program[instrPointer + 2]} ${program[instrPointer + 3]} $registers")
                    registers[program[instrPointer + 1] % BASE] = (valueOrRegister(program[instrPointer + 2], registers) + valueOrRegister(program[instrPointer + 3], registers)) % BASE
                    instrPointer += 4
                }
                Opcodes.EQ -> {
                    if (debug) System.err.println("EQ ${program[instrPointer + 1]} ${program[instrPointer + 2]} ${program[instrPointer + 3]} $registers")
                    registers[program[instrPointer + 1] % BASE] = if (valueOrRegister(program[instrPointer + 2], registers) == valueOrRegister(program[instrPointer + 3], registers)) 1 else 0
                    instrPointer += 4
                }
                Opcodes.PUSH -> {
                    if (debug) System.err.println("PUSH ${program[instrPointer + 1]} $registers")
                    stack.push(valueOrRegister(program[instrPointer + 1], registers))
                    instrPointer += 2
                }
                Opcodes.POP -> {
                    if (debug) System.err.println("POP ${program[instrPointer + 1]} $registers")
                    registers[program[instrPointer + 1] % BASE] = stack.pop()
                    instrPointer += 2
                }
                Opcodes.GT -> {
                    if (debug) System.err.println("GT ${program[instrPointer + 1]} ${program[instrPointer + 2]} ${program[instrPointer + 3]} $registers")
                    registers[program[instrPointer + 1] % BASE] = if (valueOrRegister(program[instrPointer + 2], registers) > valueOrRegister(program[instrPointer + 3], registers)) 1 else 0
                    instrPointer += 4
                }
                Opcodes.AND -> {
                    if (debug) System.err.println("AND ${program[instrPointer + 1]} ${program[instrPointer + 2]} ${program[instrPointer + 3]} $registers")
                    registers[program[instrPointer + 1] % BASE] = (valueOrRegister(program[instrPointer + 2], registers).and(valueOrRegister(program[instrPointer + 3], registers))) % BASE
                    instrPointer += 4
                }
                Opcodes.OR -> {
                    if (debug) System.err.println("OR ${program[instrPointer + 1]} ${program[instrPointer + 2]} ${program[instrPointer + 3]} $registers")
                    registers[program[instrPointer + 1] % BASE] = (valueOrRegister(program[instrPointer + 2], registers).or(valueOrRegister(program[instrPointer + 3], registers))) % BASE
                    instrPointer += 4
                }
                Opcodes.NOT -> {
                    if (debug) System.err.println("NOT ${program[instrPointer + 1]} ${program[instrPointer + 2]} $registers")
                    registers[program[instrPointer + 1] % BASE] = Integer.parseInt(String.format("%15s", Integer.toBinaryString(valueOrRegister(program[instrPointer + 2], registers)))
                            .replace(' ', '0')
                            .map { if (it == '0') '1' else '0' }
                            .joinToString(""), 2) % BASE
                    instrPointer += 3
                }
                Opcodes.CALL -> {
                    if (debug) System.err.println("CALL ${program[instrPointer + 1]} $registers")
                    stack.push(instrPointer + 2)
                    instrPointer = valueOrRegister(program[instrPointer + 1], registers)
                }
                Opcodes.MULT -> {
                    if (debug) System.err.println("MULT ${program[instrPointer + 1]} ${program[instrPointer + 2]} ${program[instrPointer + 3]} $registers")
                    registers[program[instrPointer + 1] % BASE] = (valueOrRegister(program[instrPointer + 2], registers) * valueOrRegister(program[instrPointer + 3], registers)) % BASE
                    instrPointer += 4
                }
                Opcodes.MOD -> {
                    if (debug) System.err.println("MOD ${program[instrPointer + 1]} ${program[instrPointer + 2]} ${program[instrPointer + 3]} $registers")
                    registers[program[instrPointer + 1] % BASE] = (valueOrRegister(program[instrPointer + 2], registers) % valueOrRegister(program[instrPointer + 3], registers)) % BASE
                    instrPointer += 4
                }
                Opcodes.RMEM -> {
                    if (debug) System.err.println("RMEM ${program[instrPointer + 1]} ${program[instrPointer + 2]} $registers")
                    registers[program[instrPointer + 1] % BASE] = program[valueOrRegister(program[instrPointer + 2], registers) % BASE]
                    instrPointer += 3
                }
                Opcodes.WMEM -> {
                    if (debug) System.err.println("WMEM ${program[instrPointer + 1]} ${program[instrPointer + 2]} $registers")
                    program[valueOrRegister(program[instrPointer + 1], registers)] = valueOrRegister(program[instrPointer + 2], registers)
                    instrPointer += 3
                }
                Opcodes.RET -> {
                    if (debug) System.err.println("RET $registers")
                    instrPointer = stack.pop()
                }
                Opcodes.IN -> {
                    if (debug) System.err.println("IN ${program[instrPointer + 1]} $registers")
                    var read = System.`in`.read() % BASE
                    if (read == 48) { // 0
                        debug = true
                        println("Turn on debug")
                        read = System.`in`.read() % BASE
                        registers[7] = 1 // TODO find good value
                    }
                    registers[program[instrPointer + 1] % BASE] = read
                    instrPointer += 2
                }
                else -> throw RuntimeException("Unkown opcode $opcode: ${program.subList(instrPointer, instrPointer + 10)}")
            }
        }
    }

    private val BASE = 32768

    private object Opcodes {
        val SET = 1
        val PUSH = 2
        val POP = 3
        val EQ = 4
        val GT = 5
        val JMP = 6
        val JT = 7
        val JF = 8
        val ADD = 9
        val MULT = 10
        val MOD = 11
        val AND = 12
        val OR = 13
        val NOT = 14
        val RMEM = 15
        val WMEM = 16
        val CALL = 17
        val RET = 18
        val OUT = 19
        val IN = 20
        val NOOP = 21
    }
}

