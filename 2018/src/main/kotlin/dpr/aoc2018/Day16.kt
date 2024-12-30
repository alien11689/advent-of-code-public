package dpr.aoc2018

import dpr.commons.Util

object Day16 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getLinesFromFile("/16/input.txt")
        val (part1, part2) = part1And2(input)
        println(part1)
        println(part2)
    }

    private fun part1And2(lines: List<String>):Pair<Any, Any> {
        var i = 0
        val availableOperations = mutableListOf(
            Operation.Addi,
            Operation.Addr,
            Operation.Mulr,
            Operation.Muli,
            Operation.Setr,
            Operation.Seti,
            Operation.Banr,
            Operation.Bani,
            Operation.Borr,
            Operation.Bori,
            Operation.Gtir,
            Operation.Gtri,
            Operation.Gtrr,
            Operation.Eqir,
            Operation.Eqri,
            Operation.Eqrr,
        )
        var countWhenMultipleMatch = 0
        val unknownOperations = mutableListOf<UnknownOperation>()
        while (i < lines.size) {
            if (!lines[i].startsWith("Before")) {
                break
            }
            val before = lines[i].replace("[", "").replace("]", "").replace(",", "").split(Regex(" +"))
            val registers = listOf(before[1].toInt(), before[2].toInt(), before[3].toInt(), before[4].toInt())

            val operands = lines[i + 1].split(Regex(" +")).map { it.toInt() }

            val after = lines[i + 2].replace("[", "").replace("]", "").replace(",", "").split(Regex(" +"))
            val registersAfter = listOf(after[1].toInt(), after[2].toInt(), after[3].toInt(), after[4].toInt())

            val matches = availableOperations.filter {
                it.apply(operands[1], operands[2], operands[3], registers) == registersAfter
            }

            unknownOperations.add(UnknownOperation(registers, operands, registersAfter, matches.toMutableList()))

            countWhenMultipleMatch += if (matches.size >= 3) 1 else 0

            i += 4
        }
        val part1 = countWhenMultipleMatch

        val mapping = mutableMapOf<Int, Operation>()
        unknownOperations.removeAll { it.matches.isEmpty() }
        while (unknownOperations.isNotEmpty() && mapping.size != 16) {
            val operandsToRemove = mutableSetOf<Operation>()
            unknownOperations.filter { it.matches.size == 1 }.forEach {
                mapping[it.operands[0]] = it.matches[0]
                operandsToRemove.add(it.matches[0])
            }
            operandsToRemove.forEach { operand ->
                unknownOperations.forEach {
                    it.matches.remove(operand)
                }
            }
            unknownOperations.removeAll { it.matches.isEmpty() }
        }

        var registers = (0..3).map { 0 }.toMutableList()
        while (i < lines.size) {
            val cur = lines[i]
            if (cur.isEmpty()) {
                ++i
                continue
            }
            val operands = cur.split(Regex(" +")).map { it.toInt() }
            registers = mapping[operands[0]]!!.apply(operands[1], operands[2], operands[3], registers).toMutableList()
            ++i
        }
        return Pair(part1, registers[0])
    }

    sealed class Operation {
        abstract fun apply(a: Int, b: Int, c: Int, registers: List<Int>): List<Int>

        object Addr : Operation() {
            override fun apply(a: Int, b: Int, c: Int, registers: List<Int>): List<Int> {
                val result = registers.toMutableList()
                result[c] = registers[a] + registers[b]
                return result
            }
        }

        object Addi : Operation() {
            override fun apply(a: Int, b: Int, c: Int, registers: List<Int>): List<Int> {
                val result = registers.toMutableList()
                result[c] = registers[a] + b
                return result
            }
        }

        object Mulr : Operation() {
            override fun apply(a: Int, b: Int, c: Int, registers: List<Int>): List<Int> {
                val result = registers.toMutableList()
                result[c] = registers[a] * registers[b]
                return result
            }
        }

        object Muli : Operation() {

            override fun apply(a: Int, b: Int, c: Int, registers: List<Int>): List<Int> {
                val result = registers.toMutableList()
                result[c] = registers[a] * b
                return result
            }
        }

        object Banr : Operation() {

            override fun apply(a: Int, b: Int, c: Int, registers: List<Int>): List<Int> {
                val result = registers.toMutableList()
                result[c] = registers[a].and(registers[b])
                return result
            }
        }

        object Bani : Operation() {

            override fun apply(a: Int, b: Int, c: Int, registers: List<Int>): List<Int> {
                val result = registers.toMutableList()
                result[c] = registers[a].and(b)
                return result
            }
        }

        object Borr : Operation() {

            override fun apply(a: Int, b: Int, c: Int, registers: List<Int>): List<Int> {
                val result = registers.toMutableList()
                result[c] = registers[a].or(registers[b])
                return result
            }
        }

        object Bori : Operation() {
            override fun apply(a: Int, b: Int, c: Int, registers: List<Int>): List<Int> {
                val result = registers.toMutableList()
                result[c] = registers[a].or(b)
                return result
            }
        }

        object Setr : Operation() {

            override fun apply(a: Int, b: Int, c: Int, registers: List<Int>): List<Int> {
                val result = registers.toMutableList()
                result[c] = registers[a]
                return result
            }
        }

        object Seti : Operation() {

            override fun apply(a: Int, b: Int, c: Int, registers: List<Int>): List<Int> {
                val result = registers.toMutableList()
                result[c] = a
                return result
            }
        }

        object Gtir : Operation() {

            override fun apply(a: Int, b: Int, c: Int, registers: List<Int>): List<Int> {
                val result = registers.toMutableList()
                result[c] = if (a > registers[b]) 1 else 0
                return result
            }
        }

        object Gtri : Operation() {

            override fun apply(a: Int, b: Int, c: Int, registers: List<Int>): List<Int> {
                val result = registers.toMutableList()
                result[c] = if (registers[a] > b) 1 else 0
                return result
            }
        }

        object Gtrr : Operation() {

            override fun apply(a: Int, b: Int, c: Int, registers: List<Int>): List<Int> {
                val result = registers.toMutableList()
                result[c] = if (registers[a] > registers[b]) 1 else 0
                return result
            }
        }

        object Eqir : Operation() {

            override fun apply(a: Int, b: Int, c: Int, registers: List<Int>): List<Int> {
                val result = registers.toMutableList()
                result[c] = if (a == registers[b]) 1 else 0
                return result
            }
        }

        object Eqri : Operation() {

            override fun apply(a: Int, b: Int, c: Int, registers: List<Int>): List<Int> {
                val result = registers.toMutableList()
                result[c] = if (registers[a] == b) 1 else 0
                return result
            }
        }

        object Eqrr : Operation() {

            override fun apply(a: Int, b: Int, c: Int, registers: List<Int>): List<Int> {
                val result = registers.toMutableList()
                result[c] = if (registers[a] == registers[b]) 1 else 0
                return result
            }
        }
    }

    data class UnknownOperation(
        val before: List<Int>,
        val operands: List<Int>,
        val after: List<Int>,
        var matches: MutableList<Operation> = mutableListOf(),
        var operation: Operation? = null
    )

}
