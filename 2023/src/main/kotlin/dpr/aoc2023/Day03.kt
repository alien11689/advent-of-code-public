package dpr.aoc2023

object Day03 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/03/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/03/test1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    data class Point2D(val x: Int, val y: Int) {
        fun adjacents(): Set<Point2D> = setOf(
            Point2D(x - 1, y - 1),
            Point2D(x + 1, y - 1),
            Point2D(x - 1, y + 1),
            Point2D(x + 1, y + 1),
            Point2D(x - 1, y),
            Point2D(x + 1, y),
            Point2D(x, y - 1),
            Point2D(x, y + 1),
        )
    }

    private fun part1(lines: List<String>): Any {
        val signes = mutableSetOf<Point2D>()
        val numbers = mutableListOf<Pair<Int, Set<Point2D>>>()
        var curNumber = 0
        var curPoints = mutableSetOf<Point2D>()
        lines.forEachIndexed { curY, line ->
            line.forEachIndexed { curX, c ->
                when {
                    c == '.' -> {
                        if (curNumber != 0) {
                            numbers.add(curNumber to curPoints)
                            curNumber = 0
                            curPoints = mutableSetOf()
                        }
                    }

                    c.isDigit() -> {
                        curNumber = curNumber * 10 + c.digitToInt()
                        curPoints.add(Point2D(curX, curY))
                    }

                    else -> {
                        signes.add(Point2D(curX, curY))
                        if (curNumber != 0) {
                            numbers.add(curNumber to curPoints)
                            curNumber = 0
                            curPoints = mutableSetOf()
                        }
                    }
                }
            }
            if (curNumber != 0) {
                numbers.add(curNumber to curPoints)
                curNumber = 0
                curPoints = mutableSetOf()
            }
        }
        val adjacents = signes.flatMap { p -> p.adjacents() }.toSet()
        return numbers.filter { it.second.any { it in adjacents } }.sumOf { it.first }
        // 316475 is wrong
    }

    private fun part2(lines: List<String>): Any {
        val signes = mutableSetOf<Point2D>()
        val numbers = mutableListOf<Pair<Int, Set<Point2D>>>()
        var curNumber = 0
        var curPoints = mutableSetOf<Point2D>()
        lines.forEachIndexed { curY, line ->
            line.forEachIndexed { curX, c ->
                when {
                    c == '.' -> {
                        if (curNumber != 0) {
                            numbers.add(curNumber to curPoints)
                            curNumber = 0
                            curPoints = mutableSetOf()
                        }
                    }

                    c.isDigit() -> {
                        curNumber = curNumber * 10 + c.digitToInt()
                        curPoints.add(Point2D(curX, curY))
                    }

                    c == '*' -> {
                        signes.add(Point2D(curX, curY))
                        if (curNumber != 0) {
                            numbers.add(curNumber to curPoints)
                            curNumber = 0
                            curPoints = mutableSetOf()
                        }
                    }

                    else -> {}
                }
            }
            if (curNumber != 0) {
                numbers.add(curNumber to curPoints)
                curNumber = 0
                curPoints = mutableSetOf()
            }
        }
        return signes.sumOf { p ->
            val adjacents = p.adjacents()
            val nums = numbers.filter { it.second.any { it in adjacents } }.map { it.first }
            if (nums.size < 2) 0L else nums.fold(1L) { acc, cur -> acc * cur }
        }
    }
}

