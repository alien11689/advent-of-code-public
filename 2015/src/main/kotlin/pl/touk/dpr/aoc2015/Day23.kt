package pl.touk.dpr.aoc2015

object Day23 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/23/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val instr = readInput(input)
        val reg = mutableMapOf(Pair("a", 0), Pair("b", 0))
        runProgram(instr, reg)
        return reg["b"]!!
    }

    private fun readInput(input: List<String>) = input.map { it.split(Regex("[ ,]+")).toList() }

    private fun runProgram(instr: List<List<String>>, reg: MutableMap<String, Int>) {
        var i = 0
        while (i < instr.size) {
            val cur = instr[i]
            when (cur[0]) {
                "inc" -> {
                    reg[cur[1]] = reg[cur[1]]!! + 1
                    ++i
                }
                "tpl" -> {
                    reg[cur[1]] = reg[cur[1]]!! * 3
                    ++i
                }
                "hlf" -> {
                    reg[cur[1]] = reg[cur[1]]!! / 2
                    ++i
                }
                "jie" -> if (reg[cur[1]]!! % 2 == 0) i += cur[2].toInt() else i++
                "jio" -> if (reg[cur[1]]!! == 1) i += cur[2].toInt() else i++
                "jmp" -> i += cur[1].toInt()
                else -> throw RuntimeException(cur[0])
            }
        }
    }

    private fun part2(input: List<String>): Any {
        val instr = readInput(input)
        val reg = mutableMapOf(Pair("a", 1), Pair("b", 0))
        runProgram(instr, reg)
        return reg["b"]!!
    }
}
