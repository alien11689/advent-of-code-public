package pl.touk.dpr.aoc2015

object Day11 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = "cqjxjnds"
        part1And2(input)
    }

    private fun part1And2(input: String) {
        generateSequence({ input.toMutableList() }) { generateNext(it) }
                .filter {
                    var i = it.size - 1
                    while (i > 1) {
                        if (it[i] == it[i - 1] + 1 && it[i] == it[i - 2] + 2) {
                            return@filter true
                        }
                        --i
                    }
                    false
                }
                .filter {
                    var i = it.size - 1
                    var pairs = mutableSetOf<Char>()
                    while (i > 0) {
                        if (it[i] == it[i - 1]) {
                            pairs.add(it[i])
                        }
                        --i
                    }
                    pairs.size >= 2
                }
                .take(2)
                .forEach { println(it.joinToString(separator = "")) }
    }

    private fun generateNext(prev: MutableList<Char>): MutableList<Char> {
        var i = prev.size - 3
        if (prev[i] == prev[i - 1] || prev[i - 1] == prev[i - 2] || prev[i - 2] == prev[i - 3] || prev[i - 3] == prev[i - 4] || prev[i - 4] == prev[i - 5]) {
            //has pair in first part
            return incrementBy1(prev, prev.size - 1)
        } else {
            val next = incrementBy1(prev, prev.size - 3)
            next[i + 1] = 'a'
            next[i + 2] = 'a'
            return next
        }
    }

    private fun incrementBy1(prev: MutableList<Char>, pos: Int): MutableList<Char> {
        var i = pos
        while (i >= 0) {
            if (prev[i] == 'z') {
                prev[i] = 'a'
            } else {
                prev[i] = prev[i] + if (prev[i] in setOf('h', 'n', 'k')) 2 else 1
                return prev
            }
            --i
        }
        return prev
    }
}
