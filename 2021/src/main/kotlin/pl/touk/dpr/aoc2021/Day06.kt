package pl.touk.dpr.aoc2021

object Day06 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getLinesFromFile("/06/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Int {
        var m = lines[0].split(",").map { it.toInt() }.toList()
        for (i in 1..80) {
            val newM = mutableListOf<Int>()
            m.forEach {
                if (it == 0) {
                    newM.add(6)
                    newM.add(8)
                } else {
                    newM.add(it - 1)
                }
            }
            m = newM
//            println("After day $i there is ${m.size} fish and sum is")
        }
        return m.count()
    }

    private fun part2(lines: List<String>): Long {
        var m = mutableMapOf<Int, Long>()
        lines[0].split(",").map { it.toInt() }.forEach {
            m[it] = (m[it] ?: 0) + 1
        }
        for (i in 1..256) {
            val newM = mutableMapOf<Int, Long>()
            m.forEach { (key, value) ->
                if (key == 0) {
                    newM[6] = (newM[6] ?: 0) + value
                    newM[8] = (newM[8] ?: 0) + value
                } else {
                    newM[key - 1] = (newM[key - 1] ?: 0) + value
                }
            }
            m = newM
//            println("After day $i there is $m")
        }
        return m.values.sum()
    }

}

