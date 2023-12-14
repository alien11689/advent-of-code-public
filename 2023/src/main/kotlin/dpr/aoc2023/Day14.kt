package dpr.aoc2023

object Day14 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/14/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val rocks = mutableSetOf<Point2D>()
        var sum = 0L
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                when (c) {
                    '#' -> rocks.add(Point2D(x, y))
                    '.' -> {}
                    'O' -> {
                        val i = insertRock(Point2D(x, y), rocks)
                        sum += lines.size - i
                    }
                }
            }
        }
        return sum
    }

    private fun insertRock(rock: Point2D, rocks: MutableSet<Point2D>): Int {
        var cur = rock
        while (cur.y > 0) {
            val up = cur.up()
            if (up in rocks) {
                break
            }
            cur = up
        }
        rocks.add(cur)
        return cur.y
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

