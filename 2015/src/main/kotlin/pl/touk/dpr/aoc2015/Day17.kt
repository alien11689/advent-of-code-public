package pl.touk.dpr.aoc2015

object Day17 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/17/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val containers = input.map { it.toInt() }
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

    private fun part2(input: List<String>): Any {
        val containers = input.map { it.toInt() }
        val containerList = listContainers2(containers, 150, listOf())
        val minContainerListSize = containerList.map { it.size }.min()!!
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