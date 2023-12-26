package dpr.aoc2015

import dpr.commons.Util

object Day17 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/17/input.txt")
        val containers = input.map { it.toInt() }
        println(part1(containers))
        println(part2(containers))
    }

    private fun part1(containers: List<Int>): Any {
        return countContainers(containers, 150)
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

    private fun part2(containers: List<Int>): Any {
        val containerList = listContainers2(containers, 150, listOf())
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
