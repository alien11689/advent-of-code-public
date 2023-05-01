package dpr.synacorchallenge

import java.util.Stack
import kotlin.math.pow

object Main {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val bytes = javaClass.getResource("/challenge.bin")!!.readBytes()
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
            val code = a + b * 2.toDouble().pow(8.toDouble()).toInt()
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
        val printOnly = false
        while (instrPointer < program.size) {
            when (val opcode = program[instrPointer]) {
                Opcodes.HALT -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "HALT", 0)
                        continue
                    }
                    return
                }
                Opcodes.NOOP -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "NOOP", 0)
                        continue
                    }
                    instrPointer += 1
                }
                Opcodes.OUT -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "OUT", 1)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: OUT ${program[instrPointer + 1]} $registers")
                    print(valueOrRegister(program[instrPointer + 1], registers).toChar())
                    instrPointer += 2
                }
                Opcodes.JMP -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "JMP", 1)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: JMP ${program[instrPointer + 1]} $registers")
                    instrPointer = valueOrRegister(program[instrPointer + 1], registers)
                }
                Opcodes.JT -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "JT", 2)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: JT ${program[instrPointer + 1]} ${program[instrPointer + 2]} $registers")
                    if (valueOrRegister(program[instrPointer + 1], registers) != 0) {
                        instrPointer = valueOrRegister(program[instrPointer + 2], registers)
                    } else {
                        instrPointer += 3
                    }
                }
                Opcodes.JF -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "JF", 2)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: JF ${program[instrPointer + 1]} ${program[instrPointer + 2]}} $registers")
                    if (valueOrRegister(program[instrPointer + 1], registers) == 0) {
                        instrPointer = valueOrRegister(program[instrPointer + 2], registers)
                    } else {
                        instrPointer += 3
                    }
                }
                Opcodes.SET -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "SET", 2)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: SET ${program[instrPointer + 1]} ${program[instrPointer + 2]}} $registers")
                    registers[program[instrPointer + 1] % BASE] = valueOrRegister(program[instrPointer + 2], registers)
                    instrPointer += 3
                }
                Opcodes.ADD -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "ADD", 3)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: ADD ${program[instrPointer + 1]} ${program[instrPointer + 2]} ${program[instrPointer + 3]} $registers")
                    registers[program[instrPointer + 1] % BASE] = (valueOrRegister(program[instrPointer + 2], registers) + valueOrRegister(program[instrPointer + 3], registers)) % BASE
                    instrPointer += 4
                }
                Opcodes.EQ -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "EQ", 3)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: EQ ${program[instrPointer + 1]} ${program[instrPointer + 2]} ${program[instrPointer + 3]} $registers")
                    registers[program[instrPointer + 1] % BASE] = if (valueOrRegister(program[instrPointer + 2], registers) == valueOrRegister(program[instrPointer + 3], registers)) 1 else 0
                    instrPointer += 4
                }
                Opcodes.PUSH -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "PUSH", 1)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: PUSH ${program[instrPointer + 1]} $registers")
                    stack.push(valueOrRegister(program[instrPointer + 1], registers))
                    instrPointer += 2
                }
                Opcodes.POP -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "POP", 1)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: POP ${program[instrPointer + 1]} $registers")
                    registers[program[instrPointer + 1] % BASE] = stack.pop()
                    instrPointer += 2
                }
                Opcodes.GT -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "GT", 3)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: GT ${program[instrPointer + 1]} ${program[instrPointer + 2]} ${program[instrPointer + 3]} $registers")
                    registers[program[instrPointer + 1] % BASE] = if (valueOrRegister(program[instrPointer + 2], registers) > valueOrRegister(program[instrPointer + 3], registers)) 1 else 0
                    instrPointer += 4
                }
                Opcodes.AND -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "AND", 3)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: AND ${program[instrPointer + 1]} ${program[instrPointer + 2]} ${program[instrPointer + 3]} $registers")
                    registers[program[instrPointer + 1] % BASE] = (valueOrRegister(program[instrPointer + 2], registers).and(valueOrRegister(program[instrPointer + 3], registers))) % BASE
                    instrPointer += 4
                }
                Opcodes.OR -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "OR", 3)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: OR ${program[instrPointer + 1]} ${program[instrPointer + 2]} ${program[instrPointer + 3]} $registers")
                    registers[program[instrPointer + 1] % BASE] = (valueOrRegister(program[instrPointer + 2], registers).or(valueOrRegister(program[instrPointer + 3], registers))) % BASE
                    instrPointer += 4
                }
                Opcodes.NOT -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "NOT", 2)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: NOT ${program[instrPointer + 1]} ${program[instrPointer + 2]} $registers")
                    registers[program[instrPointer + 1] % BASE] = Integer.parseInt(String.format("%15s", Integer.toBinaryString(valueOrRegister(program[instrPointer + 2], registers)))
                            .replace(' ', '0')
                            .map { if (it == '0') '1' else '0' }
                            .joinToString(""), 2) % BASE
                    instrPointer += 3
                }
                Opcodes.CALL -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "CALL", 1)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: CALL ${program[instrPointer + 1]} $registers")
//                    setting R0 = 6 and skipping instruction does not give valid code...
                    if (debug && program[instrPointer + 1] == 6027) {
                        println("Solving teleporter")
                        debug = false
                        instrPointer += 2
                        registers[0] = 6
                        continue
                    }
                    stack.push(instrPointer + 2)
                    instrPointer = valueOrRegister(program[instrPointer + 1], registers)
                }
                Opcodes.MULT -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "MULT", 3)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: MULT ${program[instrPointer + 1]} ${program[instrPointer + 2]} ${program[instrPointer + 3]} $registers")
                    registers[program[instrPointer + 1] % BASE] = (valueOrRegister(program[instrPointer + 2], registers) * valueOrRegister(program[instrPointer + 3], registers)) % BASE
                    instrPointer += 4
                }
                Opcodes.MOD -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "MOD", 3)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: MOD ${program[instrPointer + 1]} ${program[instrPointer + 2]} ${program[instrPointer + 3]} $registers")
                    registers[program[instrPointer + 1] % BASE] = (valueOrRegister(program[instrPointer + 2], registers) % valueOrRegister(program[instrPointer + 3], registers)) % BASE
                    instrPointer += 4
                }
                Opcodes.RMEM -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "RMEM", 2)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: RMEM ${program[instrPointer + 1]} ${program[instrPointer + 2]} $registers")
                    registers[program[instrPointer + 1] % BASE] = program[valueOrRegister(program[instrPointer + 2], registers) % BASE]
                    instrPointer += 3
                }
                Opcodes.WMEM -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "WMEM", 2)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: WMEM ${program[instrPointer + 1]} ${program[instrPointer + 2]} $registers")
                    program[valueOrRegister(program[instrPointer + 1], registers)] = valueOrRegister(program[instrPointer + 2], registers)
                    instrPointer += 3
                }
                Opcodes.RET -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "RET", 0)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: RET $registers")
                    instrPointer = stack.pop()
                }
                Opcodes.IN -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "IN", 1)
                        continue
                    }
                    if (debug) System.err.println("$instrPointer: IN ${program[instrPointer + 1]} $registers")
                    var read = System.`in`.read() % BASE
                    if (read == 48) { // 0
                        debug = true
                        println("Turn on debug")
                        registers[7] = 25734
                        read = System.`in`.read() % BASE
                    }
                    if (read == 49) { // 1
                        println("Goodbye!!!")
                        return
                    }
                    registers[program[instrPointer + 1] % BASE] = read
                    instrPointer += 2
                }
                else -> {
                    if (printOnly) {
                        instrPointer += printCommand(program, instrPointer, "DATA $opcode", 0)
                        continue
                    }
                    throw RuntimeException("Unkown opcode $opcode: ${program.subList(instrPointer, instrPointer + 10)}")
                }
            }
        }
    }

    private fun printCommand(program: MutableList<Int>, instrPointer: Int, name: String, params: Int): Int {
        print("$instrPointer\t: $name")
        var i = 0
        while (i < params) {
            val param = program[instrPointer + 1 + i]
            if (param >= BASE) {
                print(" R${param % BASE}")
            } else {
                print(" $param")
            }
            ++i
        }
        println()
        return 1 + params
    }

    private const val BASE = 32768

    private object Opcodes {
        const val HALT = 0
        const val SET = 1
        const val PUSH = 2
        const val POP = 3
        const val EQ = 4
        const val GT = 5
        const val JMP = 6
        const val JT = 7
        const val JF = 8
        const val ADD = 9
        const val MULT = 10
        const val MOD = 11
        const val AND = 12
        const val OR = 13
        const val NOT = 14
        const val RMEM = 15
        const val WMEM = 16
        const val CALL = 17
        const val RET = 18
        const val OUT = 19
        const val IN = 20
        const val NOOP = 21
    }
}

