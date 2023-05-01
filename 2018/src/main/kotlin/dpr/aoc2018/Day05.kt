package dpr.aoc2018

object Day05 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/05/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val changes = ('a'..'z').flatMap { listOf(Regex("$it${it.uppercaseChar()}"), Regex("${it.uppercaseChar()}$it")) }

        return reduce(input, changes).length
    }

    private fun reduce(input: String, changes: List<Regex>): String {
        var before = input
        var curSize = input.length
        var nextSize = 0
        while (curSize != nextSize) {
            curSize = nextSize
            changes.forEach {
                before = before.replace(it, "")
            }
            nextSize = before.length
        }
        return before
    }

    private fun part2(input: String): Any {
        val changes = ('a'..'z').flatMap { listOf(Regex("$it${it.uppercaseChar()}"), Regex("${it.uppercaseChar()}$it")) }
        val reducing = ('a'..'z').flatMap { listOf(Regex("[$it${it.uppercaseChar()}]")) }

        val res = reduce(input, changes)

        return reducing.map { reduce(res.replace(it, ""), changes) }.minByOrNull { it.length }?.length!!
    }
}
