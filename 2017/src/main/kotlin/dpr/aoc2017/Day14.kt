package dpr.aoc2017

import dpr.commons.Point2D
import dpr.commons.Util
import java.util.Stack

object Day14 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = "ugkiagan"
        println(part1(input))
        println(part2(input))
    }

    private val map = mapOf(
        Pair('0', "0000"),
        Pair('1', "0001"),
        Pair('2', "0010"),
        Pair('3', "0011"),
        Pair('4', "0100"),
        Pair('5', "0101"),
        Pair('6', "0110"),
        Pair('7', "0111"),
        Pair('8', "1000"),
        Pair('9', "1001"),
        Pair('a', "1010"),
        Pair('b', "1011"),
        Pair('c', "1100"),
        Pair('d', "1101"),
        Pair('e', "1110"),
        Pair('f', "1111"),
    )

    private fun part1(input: String): Any {
        val grid = buildGrid(input)
        return grid.joinToString("").filter { it == '1' }.length
    }

    private fun buildGrid(input: String): List<String> {
        return ((0..127).map { Day10.knotHash("$input-$it") })
            .map {
                it.map { letter -> map[letter] }.joinToString("")
            }
    }

    private fun part2(input: String): Any {
        val grid = buildGrid(input).map { it.toMutableList() }.toMutableList()

        var currentGroup = 0

        (0 until grid.size).forEach { y ->
            (0 until grid[y].size).forEach { x ->
                if (grid[y][x] == '1') {
                    val v = currentGroup--
                    val mem = mutableSetOf<Point2D>()
                    val neighbours = Stack<Point2D>()
                    neighbours.push(Point2D(x, y))
                    while (neighbours.isNotEmpty()) {
                        val cur = neighbours.pop()
                        if (grid[cur.y][cur.x] == '1') {
                            grid[cur.y][cur.x] = v.toString()[0]
                            val n = neighbour(cur, grid.size).filter { it !in mem }
                            mem.addAll(n)
                            neighbours.addAll(n)
                        }

                    }

                }
            }
        }
        return currentGroup * (-1)
    }

    private fun neighbour(p: Point2D, size: Int): List<Point2D> {
        val limit = 0 until size
        return p.neighboursCross()
            .filter { it.x in limit && it.y in limit }
    }
}
