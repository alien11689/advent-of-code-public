package pl.touk.dpr.aoc2020

object Day17 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getLinesFromFile("/17/input.txt")
//        val input = Util.getLinesFromFile("/17/test.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val initPoints = mutableSetOf<Point>()
        input.forEachIndexed { r, line ->
            line.forEachIndexed { c, column ->
                if (column == '#') {
                    initPoints.add(Point(c, r))
                }
            }
        }
        var points = initPoints.toSet()
        (1..6).forEach {
            points = cycle(points)
        }
        return points.size
    }

    private fun part2(input: List<String>): Any {
        val initPoints = mutableSetOf<Point>()
        input.forEachIndexed { r, line ->
            line.forEachIndexed { c, column ->
                if (column == '#') {
                    initPoints.add(Point(c, r))
                }
            }
        }
        var points = initPoints.toSet()
        (1..6).forEach {
            points = cycle2(points)
        }
        return points.size
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

    fun cycle2(points: Set<Point>): Set<Point> =
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
