package pl.touk.dpr.aoc2020

object Day01 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/01/input.txt")
        part1(input)
        part2(input)
    }

    private fun part2(input: List<String>) {
        val nums = input.map { it.toInt() }
        nums.withIndex().forEach {
            val a = it.value
            val i = it.index
            nums.withIndex().filter { it.index > i }.forEach {
                val b = it.value
                val ii = it.index
                nums.withIndex().filter { it.index > ii }.forEach {
                    val c = it.value
                    if (a + b + c == 2020) {
                        println(a * b * c)
                        return
                    }
                }
            }
        }
    }

    private fun part1(input: List<String>) {
        val mem = mutableSetOf<Int>()
        println(input
                .flatMap { s ->
                    val n = s.toInt()
                    val compliant = 2020 - n;
                    if (compliant in mem) {
                        listOf(n * compliant)
                    } else {
                        mem.add(n)
                        listOf()
                    }
                }
                .take(1)[0])
    }

}

