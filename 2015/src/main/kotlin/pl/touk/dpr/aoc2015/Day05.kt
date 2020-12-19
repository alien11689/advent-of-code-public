package pl.touk.dpr.aoc2015

object Day05 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/05/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        return input
                .filter { it.filter { it in setOf('a', 'e', 'i', 'o', 'u') }.count() >= 3 }
                .filter { inp -> setOf("ab", "cd", "pq", "xy").none { inp.contains(it) } }
                .filter {
                    var i = 0
                    while (i < it.length - 1) {
                        if (it[i] == it[i + 1]) {
                            return@filter true
                        }
                        ++i
                    }
                    return@filter false
                }
                .count()
    }

    private fun part2(input: List<String>): Any {
        return input
                .filter {
                    var i = 0
                    while (i < it.length - 2) {
                        if (it[i] == it[i + 2]) {
                            return@filter true
                        }
                        ++i
                    }
                    return@filter false
                }
                .filter {
                    var i = 0
                    while (i < it.length - 2) {
                        if (it.substring(i + 2).contains(it.substring(i, i + 2))) {
                            return@filter true
                        }
                        ++i
                    }
                    return@filter false
                }
                .count()

    }
}