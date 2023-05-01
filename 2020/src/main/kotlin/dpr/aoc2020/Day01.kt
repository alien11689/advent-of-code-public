package dpr.aoc2020

object Day01 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/01/input.txt")
        part1(input)
        part2(input)
    }

    private fun part1(input: List<String>) {
        val mem = mutableSetOf<Int>()
        println(input
                .flatMap { s ->
                    val n = s.toInt()
                    val compliant = 2020 - n
                    if (compliant in mem) {
                        listOf(n * compliant)
                    } else {
                        mem.add(n)
                        listOf()
                    }
                }
                .take(1)[0])
    }

    private fun part2(input: List<String>) {
        val nums = input.map { it.toInt() }
        nums.forEachIndexed { i, a ->
            nums.withIndex().filter { it.index > i }.forEach { (ii, b) ->
                nums.withIndex().filter { it.index > ii }.forEach { (_, c) ->
                    if (a + b + c == 2020) {
                        println(a * b * c)
                        return
                    }
                }
            }
        }
    }

}

