package pl.touk.dpr.aoc2015

object Day03 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/03/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        return input.fold(listOf(Pair(0, 0))) { acc, s ->
            val cur = acc.last()
            val next = when (s) {
                '>' -> Pair(cur.first + 1, cur.second)
                '<' -> Pair(cur.first - 1, cur.second)
                '^' -> Pair(cur.first, cur.second - 1)
                'v' -> Pair(cur.first, cur.second + 1)
                else -> throw RuntimeException()
            }
            acc + next
        }.toSet().count()
    }

    private fun part2(input: String): Any {
        return IntRange(0, 1).flatMap { santa ->
            input.foldIndexed(listOf(Pair(0, 0))) { i, acc, s ->
                if (i % 2 == santa) {
                    acc
                } else {
                    val cur = acc.last()
                    val next = when (s) {
                        '>' -> Pair(cur.first + 1, cur.second)
                        '<' -> Pair(cur.first - 1, cur.second)
                        '^' -> Pair(cur.first, cur.second - 1)
                        'v' -> Pair(cur.first, cur.second + 1)
                        else -> throw RuntimeException()
                    }
                    acc + next
                }
            }
        }.toSet().size
    }
}
