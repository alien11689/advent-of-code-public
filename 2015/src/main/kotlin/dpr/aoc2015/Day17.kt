package dpr.aoc2015

import dpr.commons.Util

object Day17 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/17/input.txt")
        val containers = parseInput(input)
        println(part1(containers))
        println(part2(containers))
    }

    @JvmStatic
    fun parseInput(input: List<String>): List<Int> = input.map { it.toInt() }

    @JvmStatic
    @JvmOverloads
    fun part1(containers: List<Int>, capacity: Int = 150): Int {
        return countContainers(containers, capacity)
    }

    private fun countContainers(containers: List<Int>, capacity: Int): Int {
        if (capacity == 0) {
            return 1
        }
        if (capacity < 0) {
            return 0
        }
        if (containers.isEmpty()) {
            return 0
        }
        val current = containers.first()
        val tail = containers.drop(1)
        return countContainers(tail, capacity) + countContainers(tail, capacity - current)
    }

    @JvmStatic
    @JvmOverloads
    fun part2(containers: List<Int>, capacity: Int = 150): Int {
        val containerList = listContainers2(containers, capacity, listOf())
        val minContainerListSize = containerList.minOf { it.size }
        return containerList
            .count { it.size == minContainerListSize }
    }

    private fun listContainers2(containers: List<Int>, capacity: Int, history: List<Int>): List<List<Int>> {
        if (capacity == 0) {
            return listOf(history)
        }
        if (capacity < 0) {
            return listOf()
        }
        if (containers.isEmpty()) {
            return listOf()
        }
        val current = containers.first()
        val tail = containers.drop(1)
        return listContainers2(tail, capacity, history) + listContainers2(tail, capacity - current, history + current)
    }

}
