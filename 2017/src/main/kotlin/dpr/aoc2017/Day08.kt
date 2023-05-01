package dpr.aoc2017

object Day08 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/08/input.txt")
        part1And2(input).forEach { println(it) }
    }

    private fun part1And2(input: List<String>): Collection<Int> {
        val registers = mutableMapOf<String, Int>().withDefault { 0 }
        val highestEver = input.map { it.split(' ') }.map {
            Instruction(
                    register = it[0],
                    inc = it[1] == "inc",
                    amount = it[2].toInt(),
                    condition = Condition(
                            register = it[4],
                            sign = it[5],
                            amount = it[6].toInt()
                    )
            )
        }.fold(Integer.MIN_VALUE) { acc, instruction ->
            instruction.apply(registers, acc)
        }
        return listOf(registers.values.maxOrNull()!!, highestEver)
    }

    data class Instruction(val register: String, val inc: Boolean, val amount: Int, val condition: Condition) {
        fun apply(reg: MutableMap<String, Int>, highestEver: Int): Int {
            if (condition.match(reg)) {
                val curReg = reg[register] ?: 0
                if (inc) {
                    reg[register] = curReg + amount
                } else {
                    reg[register] = curReg - amount
                }
                return listOf(reg[register]!!, highestEver).maxOrNull()!!
            }
            return highestEver
        }
    }

    data class Condition(val register: String, val sign: String, val amount: Int) {
        fun match(reg: Map<String, Int>): Boolean {
            val regValue = reg[register] ?: 0
            return when (sign) {
                "==" -> regValue == amount
                "!=" -> regValue != amount
                ">=" -> regValue >= amount
                "<=" -> regValue <= amount
                ">" -> regValue > amount
                "<" -> regValue < amount
                else -> throw RuntimeException(sign)
            }
        }
    }
}
