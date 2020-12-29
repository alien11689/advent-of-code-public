package pl.touk.dpr.aoc2017

object Day02 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/02/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        return input
                .map { it.split('\t').map { it.toInt() } }
                .map { it.maxOrNull()!! - it.minOrNull()!! }
                .sum()
    }

    private fun part2(input: List<String>): Any {
        return input
                .map { it.split('\t').map { it.toInt() }.sorted().reversed() }
                .map { findDivisible(it) }
                .sum()
    }

    private fun findDivisible(nums: List<Int>): Int {
        for (i in nums.indices) {
            val first = nums[i]
            for (j in ((i + 1) until nums.size)) {
                if (first % nums[j] == 0) {
                    return first / nums[j]
                }
            }
        }
        return 0
    }

}