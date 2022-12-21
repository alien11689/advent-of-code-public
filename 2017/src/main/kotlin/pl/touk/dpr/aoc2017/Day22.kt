package pl.touk.dpr.aoc2017

object Day22 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/22/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(lines: List<String>): Any {
        val grid = mutableMapOf<Pair<Int, Int>, Boolean>()
        val curX = (lines[0].length) / 2
        val curY = (lines.size) / 2

        var j = 0
        lines.forEach { row ->
            var i = 0
            row.forEach { cell ->
                if (cell == '#') {
                    grid[Pair(i, j)] = true
                }
                ++i
            }
            ++j

        }

        val virus = Virus(curX, curY)
        (0 until 10000).forEach {
            virus.burst(grid)
        }

        return virus.infected

    }

    private fun part2(lines: List<String>): Any {
        val grid = mutableMapOf<Pair<Int, Int>, Status>()
        val curX = (lines[0].length) / 2
        val curY = (lines.size) / 2

        var j = 0
        lines.forEach { row ->
            var i = 0
            row.forEach { cell ->
                if (cell == '#') {
                    grid[Pair(i, j)] = Status.Infected
                }
                ++i
            }
            ++j

        }

        val virus = Virus(curX, curY)
        (0 until 10000000).forEach {
            virus.burst2(grid)
        }

        return virus.infected
    }

    enum class Status {
        Clean,
        Weakened,
        Infected,
        Flagged;

        fun next(): Status {
            return when (this) {
                Clean -> Weakened
                Weakened -> Infected
                Infected -> Flagged
                Flagged -> Clean
            }
        }

    }

    enum class Dir(val x: Int, val y: Int) {
        Left(-1, 0),
        Right(1, 0),
        Up(0, -1),
        Down(0, 1);

        fun left(): Dir {
            return when (this) {
                Left -> Down
                Up -> Left
                Right -> Up
                Down -> Right
            }
        }

        fun right(): Dir {
            return when (this) {
                Left -> Up
                Up -> Right
                Right -> Down
                Down -> Left
            }
        }

        fun reverse(): Dir {
            return when (this) {
                Left -> Right
                Up -> Down
                Right -> Left
                Down -> Up
            }
        }
    }

    data class Virus(var x: Int, var y: Int, var dir: Dir = Dir.Up, var infected: Int = 0) {
        fun burst(grid: MutableMap<Pair<Int, Int>, Boolean>) {
            val value = grid[Pair(x, y)] ?: false
            dir = if (value) dir.right() else dir.left()
            grid[Pair(x, y)] = !value
            if (grid[Pair(x, y)]!!) {
                infected++
            }
            x += dir.x
            y += dir.y
        }

        fun burst2(grid: MutableMap<Pair<Int, Int>, Status>) {
            val value = grid[Pair(x, y)] ?: Status.Clean
            when (value) {
                Status.Clean -> dir = dir.left()
                Status.Weakened -> {
                }
                Status.Infected -> dir = dir.right()
                Status.Flagged -> dir = dir.reverse()
            }
            grid[Pair(x, y)] = value.next()
            if (grid[Pair(x, y)] == Status.Infected) {
                infected++
            }
            x += dir.x
            y += dir.y
        }
    }
}
