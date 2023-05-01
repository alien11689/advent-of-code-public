package dpr.aoc2015

import java.util.Stack

object Day24Iter {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/24/input.txt")
        val (_, entalgement) = part1(input)
        println(entalgement)
        println(part2(input, entalgement).second)
    }

    private fun part1(input: List<String>): Pair<Int, Long> {
        val numbers = input.map { it.toInt() }.sortedBy { -it }
        val perBucket = numbers.sum() / 3
        return findEntaglement(numbers, perBucket, listOf(setOf(), setOf(), setOf()), numbers.size)
    }

    private fun findEntaglement(numbers: List<Int>, perBucket: Int, initBucket: List<Set<Int>>, initBestGroupSize: Int = numbers.size, initBestEntaglement: Long = Long.MAX_VALUE - 1): Pair<Int, Long> {
        val stack = Stack<State>()
        stack.push(State(numbers, initBucket))
        var bestGroupSize = initBestGroupSize
        var bestEntalgement = initBestEntaglement
        while (stack.isNotEmpty()) {
            val (nums, buckets) = stack.pop()
//            println("${stack.size} $nums $bestGroupSize $bestEntalgement")
            if (buckets.all { it.sum() == perBucket }) {
                buckets.forEach { b ->
                    if (b.size < bestGroupSize) {
                        val ent = entanglement(b)
                        bestGroupSize = b.size
                        bestEntalgement = ent
//                        println("Current: $bestEntalgement $b")
                    } else if (b.size == bestGroupSize) {
                        val ent = entanglement(b)
                        if (ent < bestEntalgement) {
                            bestEntalgement = ent
//                            println("Current: $bestEntalgement $b")
                        }
                    }
                }
            } else if (buckets.minOf { it.size } > bestGroupSize || buckets.minOf { entanglement(it) } > bestEntalgement) {
                continue
            } else {
                val n = nums.first()
                val tail = nums.drop(1)
                var addedToEmpty = false
                buckets.forEachIndexed { i, b ->
                    if (b.isEmpty()) {
                        if (!addedToEmpty) {
                            stack.push(State(tail, buckets.mapIndexed { ii, bb -> if (i == ii) bb + n else bb }))
                            addedToEmpty = true
                        }
                    } else {
                        if (b.sum() + n <= perBucket) {
                            stack.push(State(tail, buckets.mapIndexed { ii, bb -> if (i == ii) bb + n else bb }))
                        }
                    }
                }
            }

        }
        return Pair(bestGroupSize, bestEntalgement)
    }

    private fun entanglement(s: Set<Int>): Long = s.fold(1L) { acc, i -> acc * i.toLong() }

    data class State(val numbers: List<Int>, val buckets: List<Set<Int>>)

    private fun part2(input: List<String>, initBestEntalgement: Long): Pair<Int, Long> {
        val numbers = input.map { it.toInt() }.sortedBy { -it }
        val perBucket = numbers.sum() / 4
        return findEntaglement(numbers, perBucket, listOf(setOf(), setOf(), setOf(), setOf()), numbers.size / 4, initBestEntalgement)
    }
}
