package dpr.aoc2019

import dpr.commons.Util
import kotlin.math.absoluteValue

object Day16 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/16/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        var nums = input.map { it.toString().toInt() }
        val phases = 100

        val basePattern = listOf(0, 1, 0, -1)

        var p = 0

        while (p < phases) {
            ++p
            var iter = 1
            val output = mutableListOf<Int>()
            while (output.size < nums.size) {
                val pattern = generatePattern(basePattern, iter)
                var sum = 0
                for (i in nums.indices) {
                    sum += nums[i] * pattern[(i + 1) % pattern.size]
                }
                output.add(sum.absoluteValue % 10)
                ++iter
            }
            //println("$output")
            nums = output
        }
        return nums.subList(0, 8).joinToString("")

    }

    private fun generatePattern(base: List<Int>, iter: Int): List<Int> {
        val pattern = mutableListOf<Int>()
        base.forEach { p ->
            repeat(iter) {
                pattern.add(p)
            }
        }
        return pattern
    }

    private fun part2(input: String): Any {
        var nums = input.map { it.toString().toLong() }
        var offset = nums.subList(0, 7).dropWhile { it == 0L }.joinToString("").toInt()

        val times = 10000 - offset / nums.size

        val phases = 100
        offset %= nums.size

        val realInput = mutableListOf<Long>()
        repeat(times) {
            realInput.addAll(nums)
        }

        nums = realInput

        var p = 0

        while (p < phases) {
            ++p
            var sum = nums.fold(0L) { a, b -> a + b }
            val output = mutableListOf<Long>()
            for (i in nums.indices) {
                if (i > 0) {
                    sum -= nums[i - 1]
                }
                output.add(sum % 10)
            }
            nums = output
        }
        return nums.subList(offset, offset + 8).joinToString("")
    }
}
