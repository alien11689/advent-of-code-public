package pl.touk.dpr.aoc2016

class Assembunny(program: List<String>) {

    private val instructions: List<Instr> = program.map {
        val parts = it.split(" ")
        when (parts[0]) {
            "cpy" -> Instr.Cpy(parts[1], parts[2])
            "inc" -> Instr.Inc(parts[1])
            "dec" -> Instr.Dec(parts[1])
            "jnz" -> Instr.Jnz(parts[1], parts[2])
            else -> throw RuntimeException(it)
        }
    }.toMutableList()

    sealed class Instr(val arguments: Int) {

        abstract fun run(registers: MutableMap<String, Int>): Int

        data class Cpy(val from: String, val to: String) : Instr(2) {
            override fun run(registers: MutableMap<String, Int>): Int {
                registers[to] = if (registers.containsKey(from)) registers[from]!! else from.toInt()
                return 1
            }
        }

        data class Inc(val param: String) : Instr(1) {
            override fun run(registers: MutableMap<String, Int>): Int {
                registers[param] = registers[param]!! + 1
                return 1
            }
        }

        data class Dec(val param: String) : Instr(1) {
            override fun run(registers: MutableMap<String, Int>): Int {
                registers[param] = registers[param]!! - 1
                return 1
            }
        }

        data class Jnz(val value: String, val jump: String) : Instr(2) {
            override fun run(registers: MutableMap<String, Int>): Int {
                val toCompare = if (registers.containsKey(value)) registers[value]!! else value.toInt()
                if (toCompare != 0) {
                    return jump.toInt()
                }
                return 1
            }
        }
    }

    fun run(overrideRegisters: Map<String, Int> = mapOf()): Map<String, Int> {
        val registers: MutableMap<String, Int> =
                mutableMapOf(
                        Pair("a", 0),
                        Pair("b", 0),
                        Pair("c", 0),
                        Pair("d", 0),
                )
        registers.putAll(overrideRegisters)
        var i = 0
        while (i < instructions.size) {
            i += instructions[i].run(registers)
        }
        return registers
    }
}