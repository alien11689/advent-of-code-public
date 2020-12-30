package pl.touk.dpr.aoc2017

import java.util.LinkedList
import java.util.Stack

object Day14 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = "ugkiagan"
        println(part1(input))
        println(part2(input))
    }

    val map = mapOf(
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
        val grid = ((0..127).map { "$input-$it" }.map { Day10.knotHash(it) })
                .map {
                    it.map {
                        map[it]
                    }.joinToString("")
                }
        return grid
    }

    private fun part2(input: String): Any {
        val grid = buildGrid(input).map { it.toMutableList() }.toMutableList()

        var currentGroup = 0

        (0 until grid.size).forEach { i ->
            (0 until grid[i].size).forEach { j ->
                if (grid[i][j] == '1') {
                    val v = currentGroup--
                    val mem = mutableSetOf<Pair<Int,Int>>()
                    val neighbours = Stack<Pair<Int,Int>>()
                    neighbours.push(Pair(i,j))
                    while (neighbours.isNotEmpty()) {
                        val cur = neighbours.pop()
                        if (grid[cur.first][cur.second] == '1') {
                            grid[cur.first][cur.second] = v.toString()[0]
                            val n = neighbour(cur.first, cur.second, grid.size).filter { it !in mem }
                            mem.addAll(n)
                            neighbours.addAll(n)
                        }

                    }

                }
            }
        }
        return currentGroup * (-1)
    }

    private fun neighbour(i: Int, j: Int, size: Int): List<Pair<Int, Int>> {
        return listOf(
                listOf(i - 1, j),
                listOf(i + 1, j),
                listOf(i, j - 1),
                listOf(i, j + 1),
        )
                .filter { it[0] >= 0 && it[0] < size && it[1] >= 0 && it[1] < size }
                .map { Pair(it[0], it[1]) }
    }
}