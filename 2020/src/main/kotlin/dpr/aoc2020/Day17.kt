package dpr.aoc2020

import dpr.commons.Util

object Day17 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getLinesFromFile("/17/input.txt")
//        val input = Util.getLinesFromFile("/17/test.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        var points = initPoints(input)
        repeat(6) {
            points = cycle(points)
        }
        return points.size
    }

    private fun part2(input: List<String>): Any {
        var points = initPoints(input)
        repeat(6) {
            points = cycle2(points)
        }
        return points.size
    }

    private fun initPoints(input: List<String>): Set<Point> {
        val points = mutableSetOf<Point>()
        input.forEachIndexed { r, line ->
            line.forEachIndexed { c, column ->
                if (column == '#') {
                    points.add(Point(c, r))
                }
            }
        }
        return points.toSet()
    }

    data class Point(val x: Int, val y: Int, val z: Int = 0, val w: Int = 0) {
        fun neighbours(): Set<Point> {
            val n = mutableSetOf<Point>()
            (-1..1).forEach { dx ->
                (-1..1).forEach { dy ->
                    (-1..1).forEach { dz ->
                        n.add(Point(x + dx, y + dy, z + dz))
                    }
                }
            }
            n.remove(this)
            return n.toSet()
        }

        fun neighbours2(): Set<Point> {
            val n = mutableSetOf<Point>()
            (-1..1).forEach { dx ->
                (-1..1).forEach { dy ->
                    (-1..1).forEach { dz ->
                        (-1..1).forEach { dw ->
                            n.add(Point(x + dx, y + dy, z + dz, w + dw))
                        }
                    }
                }
            }
            n.remove(this)
            return n.toSet()
        }
    }

    fun cycle(points: Set<Point>): Set<Point> =
        points.flatMap { it.neighbours() }.toSet().filter { p ->
            if (p in points) {
                val n = p.neighbours()
                points.count { it in n } in setOf(2, 3)
            } else {
                val n = p.neighbours()
                points.count { it in n } == 3
            }
        }.toSet()

    private fun cycle2(points: Set<Point>): Set<Point> =
        points.flatMap { it.neighbours2() }.toSet().filter { p ->
            if (p in points) {
                val n = p.neighbours2()
                points.count { it in n } in setOf(2, 3)
            } else {
                val n = p.neighbours2()
                points.count { it in n } == 3
            }
        }.toSet()
}
