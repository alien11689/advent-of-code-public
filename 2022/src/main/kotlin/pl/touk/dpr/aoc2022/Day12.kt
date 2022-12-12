package pl.touk.dpr.aoc2022

import java.util.PriorityQueue

object Day12 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/12/input.txt")
        println("Part 1:")
//        println(part1(Util.getNotEmptyLinesFromFile("/12/test1.txt")))
        println(part1(lines))
        println("Part 2:")
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        ('a'..'z').forEach{ print(it) }
        println()
        val points = lines.flatMapIndexed { y, line -> line.mapIndexed { x, c -> Point(x, y) to c } }.toMap()
        val start = points.filter { it.value == 'S' }.keys.first()
        val target = points.filter { it.value == 'E' }.keys.first()
        val pq = PriorityQueue<State>()
        pq.offer(State(start, 'S', 0))
        val mem = mutableSetOf<Point>()
        while (pq.isNotEmpty()) {
            val cur = pq.poll()
//            println("Checking $cur")
            cur.neghbours(points)
                .forEach {
                    if (it.height == points[target]) {
                        return it.steps
                    }
                    if (it.p !in mem) {
                        mem.add(it.p)
                        pq.offer(it)
                    }
                }
        }
        throw RuntimeException()
    }

    data class State(val p: Point, val height: Char, val steps: Int) : Comparable<State> {
        override fun compareTo(other: State): Int = steps - other.steps
        fun neghbours(points: Map<Point, Char>): Set<State> {
            return p.neighbours()
                .filter {
                    val nextHeight = points[it]
                    when (height) {
                        'S', 'a' -> nextHeight in setOf('a', 'b', 'S')
                        'b' -> nextHeight in setOf('a', 'b', 'c')
                        'z', 'E' -> nextHeight in setOf('y', 'z', 'E')
                        'y' -> nextHeight in setOf('x', 'y', 'z', 'E')
                        else -> nextHeight in setOf(height - 1, height, height + 1)
                    }
                }
                .map { State(it, points[it]!!, steps + 1) }
                .toSet()
        }

    }

    data class Point(val x: Int, val y: Int) {
        fun neighbours(): Set<Point> = setOf(
            Point(x - 1, y),
            Point(x + 1, y),
            Point(x, y - 1),
            Point(x, y + 1),
        )
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

