package pl.touk.dpr.aoc2016

class Assembunny(program: List<String>) {

    private val instructions: MutableList<Instr> = program.map {
        val parts = it.split(" ")
        when (parts[0]) {
            "cpy" -> Instr.Cpy(parts[1], parts[2])
            "inc" -> Instr.Inc(parts[1])
            "dec" -> Instr.Dec(parts[1])
            "jnz" -> Instr.Jnz(parts[1], parts[2])
            "tgl" -> Instr.Tgl(parts[1])
            "out" -> Instr.Out(parts[1])
            else -> throw RuntimeException(it)
        }
    }.toMutableList()

    sealed class Instr(val arguments: Int) {

        abstract fun run(registers: MutableMap<String, Int>, instructions: MutableList<Instr>, cur: Int, emit: MutableList<Int>): Int

        data class Cpy(val from: String, val to: String) : Instr(2) {
            override fun run(registers: MutableMap<String, Int>, instructions: MutableList<Instr>, cur: Int, emit: MutableList<Int>): Int {
                registers[to] = if (registers.containsKey(from)) registers[from]!! else from.toInt()
                return 1
            }
        }

        data class Inc(val param: String) : Instr(1) {
            override fun run(registers: MutableMap<String, Int>, instructions: MutableList<Instr>, cur: Int, emit: MutableList<Int>): Int {
                registers[param] = registers[param]!! + 1
                return 1
            }
        }

        data class Dec(val param: String) : Instr(1) {
            override fun run(registers: MutableMap<String, Int>, instructions: MutableList<Instr>, cur: Int, emit: MutableList<Int>): Int {
                registers[param] = registers[param]!! - 1
                return 1
            }
        }

        data class Jnz(val value: String, val jump: String) : Instr(2) {
            override fun run(registers: MutableMap<String, Int>, instructions: MutableList<Instr>, cur: Int, emit: MutableList<Int>): Int {
                val toCompare = if (registers.containsKey(value)) registers[value]!! else value.toInt()
                if (toCompare != 0) {
                    return if (registers.containsKey(jump)) registers[jump]!! else jump.toInt()
                }
                return 1
            }
        }

        data class Tgl(val command: String) : Instr(1) {
            override fun run(registers: MutableMap<String, Int>, instructions: MutableList<Instr>, cur: Int, emit: MutableList<Int>): Int {
                val toChange = cur + if (registers.containsKey(command)) registers[command]!! else command.toInt()
                if (toChange < 0 || toChange >= instructions.size) {
                    return 1
                }
                val instr = instructions[toChange]
                instructions[toChange] = when (instr) {
                    is Cpy -> Jnz(instr.from, instr.to)
                    is Jnz -> Cpy(instr.value, instr.jump)
                    is Dec -> Inc(instr.param)
                    is Inc -> Dec(instr.param)
                    is Tgl -> Inc(instr.command)
                    is Out -> Inc(instr.to)
                }
                return 1
            }
        }

        data class Out(val to: String) : Instr(1) {
            override fun run(registers: MutableMap<String, Int>, instructions: MutableList<Instr>, cur: Int, emit: MutableList<Int>): Int {
                emit.add(if (registers.containsKey(to)) registers[to]!! else to.toInt())
                return 1
            }
        }
    }

    fun run(overrideRegisters: Map<String, Int> = mapOf(), f: (registers: MutableMap<String, Int>, Int) -> Int? = { _, _ -> null }, emitLimit: Int = 20): Pair<Map<String, Int>, List<Int>> {
        val registers: MutableMap<String, Int> =
                mutableMapOf(
                        Pair("a", 0),
                        Pair("b", 0),
                        Pair("c", 0),
                        Pair("d", 0),
                )
        registers.putAll(overrideRegisters)
        val emit = mutableListOf<Int>()
        var i = 0
        while (i < instructions.size && emit.size < emitLimit) {
            val newI = f(registers, i)
            if (newI != null) {
                i = newI
                continue
            }
//            println("$i -> ${instructions[i]} ($registers)")
            i += instructions[i].run(registers, instructions, i, emit)
        }
        return Pair(registers, emit)
    }
}