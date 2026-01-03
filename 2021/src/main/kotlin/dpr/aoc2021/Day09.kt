package dpr.aoc2021

import dpr.commons.Util
import java.util.PriorityQueue
import dpr.commons.Point2D as Point

object Day09 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/09/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    @JvmStatic
    fun part1(lines: List<String>): Int {
        val numbers = lines.map { line -> line.map { it.toString().toInt() } }
        var risks = 0
        for (y in numbers.indices) {
            for (x in numbers[y].indices) {
                val neighbours = Point(x, y).neighboursCross()
                if (neighbours
                        .filter { n -> n.y >= 0 && n.y < numbers.size && n.x >= 0 && n.x < numbers[y].size }
                        .all { numbers[it.y][it.x] > numbers[y][x] }
                ) {
                    risks += numbers[y][x] + 1
                }
            }
        }
        return risks
    }

    @JvmStatic
    fun part2(lines: List<String>): Long {
        val numbers = lines.map { line -> line.map { it.toString().toInt() } }
        val basins = mutableSetOf<MutableSet<Point>>()
        for (y in numbers.indices) {
            for (x in numbers[y].indices) {
                val curPoint = Point(x, y)
                val neighbours = curPoint.neighboursCross()
                val validNeighbours = neighbours
                    .filter { n -> n.y >= 0 && n.y < numbers.size && n.x >= 0 && n.x < numbers[y].size }
                if (validNeighbours.all { numbers[it.y][it.x] > numbers[y][x] }) {
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
            val validN = pWithValue.p.neighboursCross()
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

    data class PointWithValue(val p: Point, val v: Int) : Comparable<PointWithValue> {
        override fun compareTo(other: PointWithValue): Int {
            return v.compareTo(other.v)

        }

    }
}

