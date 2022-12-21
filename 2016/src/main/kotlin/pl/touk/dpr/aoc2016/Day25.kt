package pl.touk.dpr.aoc2016

object Day25 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/25/input.txt")
        println(part1(input))
    }

    private fun part1(input: List<String>): Any {
        var i = 0
        while (true) {
            val message = Assembunny(input).run(mapOf(Pair("a", i)), emitLimit = 20).second
            if (message == generateSequence(0) { if (it == 0) 1 else 0 }.take(20).toList()) {
                return i
            }
            ++i
        }
    }
}
