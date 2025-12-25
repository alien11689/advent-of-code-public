package dpr.aoc2018

import dpr.commons.Util

object Day11 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        println(part1())
        println(part2())
    }

    @JvmStatic
    fun part1(serialNumber: Int = 7165): String {
        val n = 300
        val m = 300

        val board = buildBoard(n, m, serialNumber)
        val racks = calculateRacks(board)
        val rack = findLargestRack(racks)
        return "${rack.x},${rack.y}"
    }

    @JvmStatic
    fun part2(serialNumber: Int = 7165): String {
        val n = 300
        val m = 300
        val board = buildBoard(n, m, serialNumber)
        val rack = calculateRacksMax(board)
        return "${rack.x},${rack.y},${rack.rackSize}"
    }

    data class Point(val x: Int, val y: Int, val sum: Int, val rackSize: Int? = null)

    private fun getFuelLevel(x: Int, y: Int, serialNumber: Int): Int {
        val rackId = x + 10L
        val powerLevel = (rackId * y + serialNumber) * rackId
        return if (powerLevel > 99) {
            val plAsString = powerLevel.toString()
            (plAsString[plAsString.length - 3].toString().toInt()) - 5
        } else {
            -5
        }
    }

    private fun buildBoard(n: Int, m: Int, serialNumber: Int): MutableList<MutableList<Int>> {
        return (1..n).map { y ->
            (1..m).map { x ->
                getFuelLevel(x, y, serialNumber)
            }.toMutableList()
        }.toMutableList()
    }

    private fun calculateRacks(board: MutableList<MutableList<Int>>): List<Point> {
        return (1..(board.size - 2)).map { y ->
            (1..(board[0].size - 2)).map { x ->
                val ix = x - 1
                val iy = y - 1
                val sum = (iy..(iy + 2)).sumOf { i ->
                    (ix..(ix + 2)).sumOf { j -> board[i][j] }
                }
                Point(x, y, sum)
            }
        }.flatten()
    }

    private fun findLargestRack(racks: List<Point>) =
        racks.maxByOrNull { it.sum }!!

    private fun calculateRacksMax(board: MutableList<MutableList<Int>>): Point {
        var maxFuel = -1000000
        var currentMax: Point? = null
        for (y in 1..board.size) {
            for (x in 1..board.size) {
                var rackSize = 0
                var current = Point(x, y, board[y - 1][x - 1], ++rackSize)
                while (rackSize + x <= board.size && rackSize + y <= board.size) {
                    val additionalX = x + rackSize
                    val additionalY = y + rackSize
                    var newSize = current.sum
                    for (i in y..additionalY) {
                        newSize += board[i - 1][additionalX - 1]
                    }

                    for (i in x..additionalX) {
                        newSize += board[additionalY - 1][i - 1]
                    }
                    newSize -= board[additionalY - 1][additionalX - 1]
                    current = Point(x, y, newSize, rackSize + 1)
//                        println("current $current")
                    if (current.sum > maxFuel) {
                        maxFuel = current.sum
                        currentMax = current
                    }
                    rackSize++
                }
            }
        }
        return currentMax!!
    }
}
