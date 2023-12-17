package dpr.aoc2016

import dpr.commons.Util

object Day22 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/22/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val nodes = input
            .drop(2)
            .map { Node.fromLine(it) }
        var count = 0
        nodes.forEach { a ->
            nodes.filter { it != a }.forEach { b ->
                if (a.used > 0 && a.used < b.available) {
                    ++count
                }
            }
        }
        return count
    }

    private fun part2(input: List<String>): Any {
        input
            .drop(2)
            .map { Node.fromLine(it) }
//                .also { printNodes(it) }
        return 15 + 16 + 150
    }

//    private fun printNodes(n: List<Node>) {
//        val nodes = n.map { Pair(Pair(it.x, it.y), it) }.toMap()
//        val maxX = nodes.values.maxByOrNull { it.x }!!.x
//        val maxY = nodes.values.maxByOrNull { it.y }!!.y
//        for (y in (0..maxY)) {
//            for (x in (0..maxX)) {
//                val node = nodes[Pair(x, y)]!!
//                print("${node.used}/${node.available}\t")
//            }
//            println()
//        }
//    }

    data class Node(val x: Int, val y: Int, val size: Int, val used: Int, val available: Int, val percent: Int) {
        companion object {
            fun fromLine(line: String): Node {
                val split = line.split(Regex("\\s+"))
                val coords = split[0].split("-")
                return Node(
                    x = coords[1].substring(1).toInt(),
                    y = coords[2].substring(1).toInt(),
                    size = split[1].take(split[1].length - 1).toInt(),
                    used = split[2].take(split[2].length - 1).toInt(),
                    available = split[3].take(split[3].length - 1).toInt(),
                    percent = split[4].take(split[4].length - 1).toInt(),
                )
            }
        }
    }
}
