package dpr.aoc2015

import dpr.commons.Util

object Day24 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/24/input.txt")
        val numbers = input.map { it.toInt() }.sortedBy { -it }
        println(part1(numbers))
        println(part2(numbers))
    }

    private fun part1(numbers: List<Int>): Any {
        val perBucket = numbers.sum() / 3
        return bestEntanglement(numbers, perBucket)
    }

    private fun bestEntanglement(numbers: List<Int>, perBucket: Int): Long {
        val mem = mutableMapOf<Pair<List<Int>, Int>, Set<List<Int>>>()
        val possibleBuckets = findPossibleBuckets(numbers, perBucket, mem)
            .map { it.toSet() }
        val minSize = possibleBuckets.minOf { it.size }
        return possibleBuckets.filter { it.size == minSize }
            .minOf { it.fold(1L) { acc, a -> acc * a } }
    }

    private fun findPossibleBuckets(numbers: List<Int>, perBucket: Int, mem: MutableMap<Pair<List<Int>, Int>, Set<List<Int>>>): Set<List<Int>> {
        if (numbers.isEmpty()) {
            return emptySet()
        }
        val key = numbers to perBucket
        if (key in mem) {
            return mem[key]!!
        }
        val first = numbers.first()
        val tail = numbers.drop(1)
        val options = mutableSetOf<List<Int>>()
        options.addAll(findPossibleBuckets(tail, perBucket, mem))
        if (first == perBucket) {
            options.add(listOf(first))
        } else if (first <= perBucket) {
            findPossibleBuckets(tail, perBucket - first, mem)
                .forEach { options.add(it + first) }
        }
        mem[key] = options
        return options
    }

    private fun part2(numbers: List<Int>): Any {
        val perBucket = numbers.sum() / 4
        return bestEntanglement(numbers, perBucket)
    }
}
