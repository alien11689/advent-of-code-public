package pl.touk.dpr.aoc2022

object Day25 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/25/input.txt")
        println("Part 1:")
        println(part1(Util.getNotEmptyLinesFromFile("/25/test1.txt")))
        println(part1(lines))
    }

    private fun part1(lines: List<String>): Any {
        val sum = lines.sumOf { line ->
            var base = 1L
            var value = 0L
//            println("reading line $line as ${line.reversed().toCharArray().map { it }}")
            line.reversed().toCharArray().forEach {
                val number = when (it) {
                    '1' -> 1
                    '2' -> 2
                    '0' -> 0
                    '-' -> -1
                    '=' -> -2
                    else -> throw RuntimeException()
                }
//                println("Current $value plus $base * $number")
                value += base * number
                base *= 5
            }
//            println("$line => $value ")
            value
        }
        println(sum)
        var base = 1L
        while (sum > base) {
            base *= 5
        }
        base -= 1
        println("Base $base")
        TODO()
    }
}

