package dpr.aoc2021

import dpr.commons.Util
import java.util.PriorityQueue

object Day09 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/09/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val numbers = lines.map { line -> line.map { it.toString().toInt() } }
        var risks = 0
        for (i in numbers.indices) {
            for (j in numbers[i].indices) {
                val neighbours = neigh(i, j)
                if (neighbours
                        .filter { n -> n.y >= 0 && n.y < numbers.size && n.x >= 0 && n.x < numbers[i].size }
                        .all { numbers[it.y][it.x] > numbers[i][j] }
                ) {
                    risks += numbers[i][j] + 1
                }
            }
        }
        return risks
    }

    private fun neigh(i: Int, j: Int): Set<Point> {
        return setOf(
            Point(i, j + 1),
            Point(i, j - 1),
            Point(i - 1, j),
            Point(i + 1, j),
        )
    }

    private fun part2(lines: List<String>): Any {
        val numbers = lines.map { line -> line.map { it.toString().toInt() } }
        val basins = mutableSetOf<MutableSet<Point>>()
        for (i in numbers.indices) {
            for (j in numbers[i].indices) {
                val curPoint = Point(i, j)
                val neighbours = neigh(i, j)
                val validNeighbours = neighbours
                    .filter { n -> n.y >= 0 && n.y < numbers.size && n.x >= 0 && n.x < numbers[i].size }
                if (validNeighbours.all { numbers[it.y][it.x] > numbers[i][j] }) {
//                    println("Start basin from $curPoint and its neighbours $validNeighbours")
                    val basin = checkBasin(curPoint, numbers)
                    basins += basin
                }
            }
        }
        return basins.sortedBy { -it.size }.take(3).foldRight(1L) { p, acc -> p.size * acc }
    }

    private fun checkBasin(curPoint: Point, numbers: List<List<Int>>): MutableSet<Point> {
        val basin = mutableSetOf<Point>()
        basin.add(curPoint)
        val pq = PriorityQueue<PointWithValue>()
        pq.offer(PointWithValue(curPoint, numbers[curPoint.y][curPoint.x]))
        while (!pq.isEmpty()) {
            val pWithValue = pq.poll()
            if (pWithValue.v == 9) {
                continue
            }
            val validN = neigh(pWithValue.p.y, pWithValue.p.x)
                .filter { n -> n.y >= 0 && n.y < numbers.size && n.x >= 0 && n.x < numbers[0].size && n !in basin }
            //                        println("checking $pWithValue and its neigh are $validN")
            if (validN.all { numbers[it.y][it.x] >= numbers[pWithValue.p.y][pWithValue.p.x] }) {
                //                            println("Adding $pWithValue to basin")
                basin.add(pWithValue.p)
                validN.forEach { pq.offer(PointWithValue(it, numbers[it.y][it.x])) }
            }
        }
        return basin
    }

    data class Point(val y: Int, val x: Int)

    data class PointWithValue(val p: Point, val v: Int) : Comparable<PointWithValue> {
        override fun compareTo(other: PointWithValue): Int {
            return v.compareTo(other.v)

        }

    }
}

