package pl.touk.dpr.aoc2021

object Day11 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/11/input3.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val numbers = lines.map { it.map { it.toString().toInt() }.toMutableList() }.toMutableList()
        printBoard(numbers)
        var flashesCount = 0L
        val flashed = tick1(numbers)
        flashesCount += flashed.size
        printBoard(numbers)
        println(flashesCount)

        flashesCount += tick1(numbers).size
        printBoard(numbers)
        println(flashesCount)
        return -1
    }

    private fun tick1(numbers: MutableList<MutableList<Int>>): MutableSet<Point> {
        val flashed = mutableSetOf<Point>()
        for (y in numbers.indices) {
            for (x in numbers[y].indices) {
                numbers[y][x] += 1
                if (numbers[y][x] > 9) {
                    flashed += Point(x, y)
                }
            }
        }
        if (flashed.isNotEmpty()) {
            var prevFlashed = setOf<Point>()
            while (flashed.size > prevFlashed.size) {
                println("while...")
                println("flash: ${flashed - prevFlashed}")
                printBoard(numbers.map { it.map { if (it > 9) 'X' else it.toString() } })
                (flashed - prevFlashed).flatMap { neigh(it) }
                    .forEach { numbers[it.y][it.x] += 1 }
                prevFlashed = flashed.toSet()
                for (y in numbers.indices) {
                    for (x in numbers[y].indices) {
                        val cur = Point(x, y)
                        if (numbers[y][x] > 9 && cur !in flashed) {
                            flashed += cur
                        }
                    }
                }
            }
        }
        flashed.forEach { numbers[it.y][it.x] = 0 }
        return flashed
    }

    private fun printBoard(numbers: List<List<Any>>) {
        println(numbers.map { it.joinToString("") }.joinToString("\n"))
        println("=======================")
    }

    private fun part2(lines: List<String>): Any {
        return -1
    }

    data class Point(val x: Int, val y: Int)

    private fun neigh(p: Point): Set<Day09.Point> {
        return setOf(
            Day09.Point(p.x, p.y + 1),
            Day09.Point(p.x, p.y - 1),
            Day09.Point(p.x + 1, p.y - 1),
            Day09.Point(p.x + 1, p.y),
            Day09.Point(p.x + 1, p.y + 1),
            Day09.Point(p.x - 1, p.y + 1),
            Day09.Point(p.x - 1, p.y - 1),
            Day09.Point(p.x - 1, p.y),
        )
            .filter { it.x >= 0 && it.x < 10 && it.y >= 0 && it.y < 10 }
            .toSet()
    }
}

