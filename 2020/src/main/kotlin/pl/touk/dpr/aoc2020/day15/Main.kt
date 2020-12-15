package pl.touk.dpr.aoc2020.day15

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = "1,2,16,19,18,0"
        println(part1And2(input))
    }

    private fun part1And2(input: String): Any {
        val mem = mutableMapOf<Int, Int>()
        var current = 0
        input.split(",").mapIndexed { index, s ->
            mem[s.toInt()] = index + 1
            current = s.toInt()
        }
        var round = mem.size
        var res = mutableListOf<Int>()
        while (round < 30000000 + 1) {
            if (round == 2020 || round == 30000000) {
//                println("round: $round, current: $current")
                res.add(current)
            }
            if (current in mem) {
                val newCurrent = round - mem[current]!!
                mem[current] = round
                current = newCurrent
            } else {
                mem[current] = round
                current = 0

            }
            round++
        }
        return res.joinToString("\n")

    }
}

