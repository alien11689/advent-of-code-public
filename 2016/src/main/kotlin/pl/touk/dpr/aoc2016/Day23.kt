package pl.touk.dpr.aoc2016

object Day23 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/23/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        return Assembunny(input).run(mapOf(Pair("a", 7))).first["a"]!!
    }

    private fun part2(input: List<String>): Any {
        return Assembunny(input).run(mapOf(Pair("a", 12)), f = { registers, cur ->
            if (cur == 5 && registers["b"]!! > 0 && registers["c"]!! > 0 && registers["d"]!! > 0) {
                registers["a"] = registers["c"]!! * registers["d"]!!
                registers["c"] = 0
                registers["d"] = 1
                7
            } else {
                null
            }
        }).first["a"]!!
    }
}