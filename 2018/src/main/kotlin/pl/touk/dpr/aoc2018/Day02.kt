package pl.touk.dpr.aoc2018

object Day02 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/02/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        var count2 = 0
        var count3 = 0

        input.forEach { line ->
            val m = mutableMapOf<Char, Int>()
            line.forEach { letter ->
                m[letter] = m.getOrDefault(letter, 0) + 1
            }
            count2 += if (2 in m.values) 1 else 0
            count3 += if (3 in m.values) 1 else 0

        }

        return count2 * count3
    }

    private fun part2(lines: List<String>): Any {
        (0 until (lines.size - 1)).forEach { i ->
            val a = lines[i]
            ((i + 1) until (lines.size)).forEach { j ->
                val b = lines[j]
                var diff = 0
                (0 until a.length).forEach { k ->
                    if (a[k] != b[k]) {
                        ++diff
                        if (diff > 1) {
                            return@forEach
                        }
                    }

                }
                if (diff == 1) {
                    val c = (0 until a.length).map { if (a[it] == b[it]) a[it] else "" }.joinToString("")
                    return c
                }
            }
        }
        throw RuntimeException()
    }
}
