package pl.touk.dpr.aoc2016

object Day03 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/03/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        return input
                .asSequence()
                .map { it.trim().split(Regex("[ ]+")).map { it.toInt() } }
                .count { isTriangle(it) }
    }

    private fun isTriangle(sides: List<Int>): Boolean {
        val a = sides[0]
        val b = sides[1]
        val c = sides[2]
        return a + b > c && a + c > b && b + c > a
    }

    private fun part2(input: List<String>): Any {
        val triples = input
                .asSequence()
                .map { it.trim().split(Regex("[ ]+")).map { it.toInt() } }
        return (0..2)
                .flatMap { col -> triples.map { it[col] } }
                .chunked(3)
                .count { isTriangle(it) }
    }
}