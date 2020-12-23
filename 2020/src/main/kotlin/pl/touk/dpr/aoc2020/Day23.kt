package pl.touk.dpr.aoc2020

object Day23 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = "186524973"
//        val input = "389125467" //sample
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        var cups = input.map { it.toString().toInt() }.toList()
//        println(cups)
        var current = 0
        (1..100).forEach {
            cups = round(cups, current)
//            println("After round $it: $cups")
        }

        // 197865243 is not right
        // 978652431 is not right
        return cups.joinToString("") { it.toString() }
    }

    private fun round(cups: List<Int>, current: Int): List<Int> {
        val curVal = cups[current]
        val pickedUp = (1..3).map { cups[it % cups.size] }
        var destination = if (curVal == 1) 9 else (curVal - 1)
        while (destination in pickedUp) {
            destination = if (destination == 1) 9 else (destination - 1)
        }
        val cupsNew = cups.filter { it != curVal && it !in pickedUp }
        val ml = mutableListOf<Int>()
        cupsNew.forEach {
            ml.add(it)
            if (it == destination) {
                ml.addAll(pickedUp)
            }
        }
        ml.add(curVal)
        return ml.toList()
    }

    private fun part2(input: String): Any {
//        var cups = input.map { it.toString().toInt() }.toList()
////        println(cups)
//        var current = 0
//        (1..10000000).forEach {
//            cups = round(cups, current)
////            println("After round $it: $cups")
//        }
//        return cups.joinToString("") { it.toString() }
        return 0
    }
}
