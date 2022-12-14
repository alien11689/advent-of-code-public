package pl.touk.dpr.aoc2022

object Day14 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/14/input.txt")
        println("Part 1:")
        println(part1(Util.getNotEmptyLinesFromFile("/14/test1.txt")))
        println(part1(lines))
        println("Part 2:")
        println(part2(Util.getNotEmptyLinesFromFile("/14/test1.txt")))
        println(part2(lines))
    }

    data class Point(val x: Int, val y: Int) {
        fun down(): Point = copy(y = y + 1)
        fun left(): Point = copy(x = x - 1, y = y + 1)
        fun right(): Point = copy(x = x + 1, y = y + 1)
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
            val left = curSand.left()
            val right = curSand.right()
            if (down in points && left in points && right in points) {
                points.add(curSand)
                ++seen
                curSand = Point(500, 0)
                continue
            } else if (down !in points) {
                curSand = down
            } else if (left !in points) {
                curSand = left
            } else if (right !in points) {
                curSand = right
            } else {
                throw RuntimeException()
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
            val left = curSand.left()
            val right = curSand.right()
            if (down in points && left in points && right in points) {
                points.add(curSand)
                ++seen
                curSand = Point(500, 0)
                if (curSand in points) {
                    break
                }
                continue
            } else if (down !in points) {
                curSand = down
            } else if (left !in points) {
                curSand = left
            } else if (right !in points) {
                curSand = right
            } else {
                throw RuntimeException()
            }
        }
        return seen
    }

    private fun extendPoints(maxY: Int, points: MutableSet<Point>) {
        val startX = 500
        var l = startX
        var r = startX
        var h = 0
        while (h != maxY) {
            h += 1
            l--
            r++
        }
        println("Expected border x is $l to $r")
        for (x in (l..r)) {
            points.add(Point(x, maxY))
        }
    }

    private fun printlnBoard(points: MutableSet<Point>) {
        val minX = points.map { it.x }.min()
        val maxX = points.map { it.x }.max()
        val minY = points.map { it.y }.min()
        val maxY = points.map { it.y }.max()
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
            var i = 1
            while (i < parts.size) {
                val (destX, destY) = parts[i].split(",").map { it.toInt() }
                val dx = if (curX == destX) 0 else if (curX < destX) 1 else -1
                val dy = if (curY == destY) 0 else if (curY < destY) 1 else -1
                while (curX != destX || curY != destY) {
                    curX += dx
                    curY += dy
                    points.add(Point(curX, curY))
                }
                ++i
            }
        }
        return points
    }
}

