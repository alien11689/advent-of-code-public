package pl.touk.dpr.aoc2022

object Day14 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/14/input.txt")
        println("Part 1:")
//        println(part1(Util.getNotEmptyLinesFromFile("/14/test1.txt")))
        println(part1(lines))
        println("Part 2:")
//        println(part2(Util.getNotEmptyLinesFromFile("/14/test1.txt")))
        println(part2(lines))
    }

    data class Point(val x: Int, val y: Int) {
        fun down(): Point = copy(y = y + 1)
        fun leftDown(): Point = copy(x = x - 1, y = y + 1)
        fun rightDown(): Point = copy(x = x + 1, y = y + 1)
    }

    private fun part1(lines: List<String>): Any {
        val points = readPoints(lines)
        //printlnBoard(points)
        val maxY = points.maxOfOrNull { it.y }!!
        var curSand = Point(500, 0)
        var seen = 0
        while (true) {
//            println("Checking $curSand")
            val down = curSand.down()
            val leftDown = curSand.leftDown()
            val rightDown = curSand.rightDown()
            if (down !in points) {
                curSand = down
            } else if (leftDown !in points) {
                curSand = leftDown
            } else if (rightDown !in points) {
                curSand = rightDown
            } else {
                points.add(curSand)
                ++seen
                curSand = Point(500, 0)
                continue
            }
            if (curSand.y == maxY) {
                break
            }
        }
        return seen
    }

    private fun part2(lines: List<String>): Any {
        val points = readPoints(lines)
//        printlnBoard(points)
        val maxY = points.map { it.y }.max() + 2
        extendPoints(maxY, points)
        var curSand = Point(500, 0)
        var seen = 0
        while (true) {
//            println("Checking $curSand")
            val down = curSand.down()
            val leftDown = curSand.leftDown()
            val rightDown = curSand.rightDown()
            if (down !in points) {
                curSand = down
            } else if (leftDown !in points) {
                curSand = leftDown
            } else if (rightDown !in points) {
                curSand = rightDown
            } else {
                points.add(curSand)
                ++seen
                curSand = Point(500, 0)
                if (curSand in points) {
                    break
                }
            }
        }
        return seen
    }

    private fun extendPoints(maxY: Int, points: MutableSet<Point>) {
        val startX = 500
        val l = startX - maxY
        val r = startX + maxY
//        println("Expected border x is $l to $r")
        (l..r).forEach { x ->
            points.add(Point(x, maxY))
        }
    }

    private fun printlnBoard(points: MutableSet<Point>) {
        val minX = points.minOf { it.x }
        val maxX = points.maxOf { it.x }
        val minY = points.minOf { it.y }
        val maxY = points.maxOf { it.y }
        for (y in minY..maxY) {
            for (x in minX..maxX) {
                print(if (Point(x, y) in points) "#" else ".")
            }
            println()
        }
    }

    private fun readPoints(lines: List<String>): MutableSet<Point> {
        val points = mutableSetOf<Point>()
        lines.forEach { line ->
            val parts = line.split(" -> ")
            var (curX, curY) = parts[0].split(",").map { it.toInt() }
            points.add(Point(curX, curY))
            (1 until parts.size).forEach { i ->
                val (destX, destY) = parts[i].split(",").map { it.toInt() }
                val dx = if (curX == destX) 0 else if (curX < destX) 1 else -1
                val dy = if (curY == destY) 0 else if (curY < destY) 1 else -1
                while (curX != destX || curY != destY) {
                    curX += dx
                    curY += dy
                    points.add(Point(curX, curY))
                }
            }
        }
        return points
    }
}

