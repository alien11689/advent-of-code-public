package dpr.aoc2016

object Day20 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/20/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val ranges = parseInput(input)
        var minNotExcluded = 0L
        ranges.forEach { range ->
            if (range.from <= minNotExcluded && range.to >= minNotExcluded) {
                minNotExcluded = range.to + 1
            }
            if (range.from > minNotExcluded) {
                return minNotExcluded
            }
        }
        return -1
    }

    private fun parseInput(input: List<String>) = input.map {
        val parts = it.split("-")
        IpRange(parts[0].toLong(), parts[1].toLong())
    }.sorted()

    private fun part2(input: List<String>): Any {
        val ranges = parseInput(input)
        var minNotExcluded = 0L
        val notExcluded = mutableListOf<Long>()
        ranges.forEach { range ->
            if (range.from <= minNotExcluded && range.to >= minNotExcluded) {
                minNotExcluded = range.to + 1
            } else if (range.from > minNotExcluded) {
                while (range.from > minNotExcluded) {
                    notExcluded.add(minNotExcluded)
                    ++minNotExcluded
                }
                minNotExcluded = range.to + 1
            }
        }
        return notExcluded.size
    }

    data class IpRange(val from: Long, val to: Long) : Comparable<IpRange> {
        override fun compareTo(other: IpRange): Int {
            return from.compareTo(other.from)
        }
    }
}
