package dpr.aoc2017

import dpr.commons.Util

object Day07 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/07/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val (discs, allChildren) = parseInput(input)

        return (discs.keys - allChildren).first()
    }

    private fun parseInput(input: List<String>): Pair<MutableMap<String, Disc>, MutableSet<String>> {
        val discs = mutableMapOf<String, Disc>()
        val allChildren = mutableSetOf<String>()

        input.forEach { line ->
            val parts = line.split(" ", limit = 4)
            val disc = Disc(parts[0],
                parts[1].removePrefix("(").removeSuffix(")").toInt(),
                if (parts.size > 2) parts[3].split(',').map { it.trim() } else listOf()
            )
            discs[parts[0]] = disc
            allChildren.addAll(disc.children)
        }
        return Pair(discs, allChildren)
    }

    private fun part2(input: List<String>): Any {
        val (discs, allChildren) = parseInput(input)
        val root = (discs.keys - allChildren).first()
        var cur = discs[root]!!
        var expectedSize = 0
        while (true) {
            if (cur.isUnbalanced(discs)) {
                val eachCount = cur.children.map { discs[it]!!.wholeSize(discs) }.groupingBy { it }.eachCount()
                val oddDiscSize = eachCount.minByOrNull { it.value }!!.key
                cur = discs[cur.children.find { discs[it]!!.wholeSize(discs) == oddDiscSize }!!]!!
                expectedSize = eachCount.maxByOrNull { it.value }!!.key
            } else {
                return expectedSize - cur.children.sumOf { discs[it]!!.wholeSize(discs) }
            }
        }
    }

    data class Disc(val name: String, val size: Int, val children: List<String>) {
        var sum = -1

        fun wholeSize(discs: Map<String, Disc>): Int {
            if (sum >= 0) {
                return sum
            }
            sum = size + (children.sumOf { discs[it]!!.wholeSize(discs) })
            return sum
        }

        fun isUnbalanced(discs: Map<String, Disc>): Boolean {
            if (children.isEmpty()) {
                return false
            }
            val sizes = children.map { discs[it]!!.wholeSize(discs) }.toSet()
            return sizes.size != 1
        }
    }
}
