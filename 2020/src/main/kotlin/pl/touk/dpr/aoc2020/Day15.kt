package pl.touk.dpr.aoc2020

object Day15 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
//        Util.test(part1And2("0,3,6"), listOf(436, 175594))
//        Util.test(part1And2("1,3,2"), listOf(1, 2578))
//        Util.test(part1And2("2,1,3"), listOf(10, 3544142))
//        Util.test(part1And2("1,2,3"), listOf(27, 261214))
//        Util.test(part1And2("2,3,1"), listOf(78, 6895259))
//        Util.test(part1And2("3,2,1"), listOf(438, 18))
//        Util.test(part1And2("3,1,2"), listOf(1836, 362))
        val input = "1,2,16,19,18,0"
        println(part1And2(input).joinToString("\n"))
    }

    private fun part1And2(input: String): List<Int> {
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
        return res
    }
}

