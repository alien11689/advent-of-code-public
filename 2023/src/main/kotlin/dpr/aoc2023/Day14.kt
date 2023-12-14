package dpr.aoc2023

object Day14 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/14/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/14/test1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val (rocks, movableRocks) = readInput(lines)
        val next = moveRocks(movableRocks, Dir.N, rocks, lines[0].length, lines.size)
        return calculateLoad(next, lines.size)
    }

    private fun part2(lines: List<String>): Any {
        val (rocks, movableRocks) = readInput(lines)
        val maxCycles = 1000000000
        val sizeY = lines.size
        val sizeX = lines[0].length
        val memory = mutableMapOf<Set<Point2D>, Int>()
        var current = movableRocks.toSet()
        memory[current] = 0
        var i = 0
        while (i < maxCycles) {
            ++i
            current = cycle(current, rocks, sizeX, sizeY)
            memory[current]?.let { prev ->
                val diff = i - prev
                i += (maxCycles - i) / diff * diff
                memory.clear()
            }
            memory[current] = i
        }
        return calculateLoad(current, sizeY)
    }

    private fun readInput(lines: List<String>): Pair<Set<Point2D>, Set<Point2D>> {
        val rocks = mutableSetOf<Point2D>()
        val movableRocks = mutableSetOf<Point2D>()
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                when (c) {
                    '#' -> rocks.add(Point2D(x, y))
                    'O' -> movableRocks.add(Point2D(x, y))
                    else -> {}
                }
            }
        }
        return Pair(rocks, movableRocks)
    }

    private fun calculateLoad(current: Set<Point2D>, sizeY: Int): Int = current.sumOf { sizeY - it.y }

    private fun cycle(start: Set<Point2D>, rocks: Set<Point2D>, sizeX: Int, sizeY: Int): Set<Point2D> {
        return listOf(Dir.N, Dir.W, Dir.S, Dir.E).fold(start) { cur: Set<Point2D>, dir: Dir -> moveRocks(cur, dir, rocks, sizeX, sizeY) }
    }

    private fun moveRocks(start: Set<Point2D>, dir: Dir, rocks: Set<Point2D>, sizeX: Int, sizeY: Int): Set<Point2D> {
        val res = mutableSetOf<Point2D>()
        when (dir) {
            Dir.N -> {
                start.sortedBy { it.y }.forEach {
                    var cur = it
                    while (cur.y > 0) {
                        val next = cur.up()
                        if (next in rocks || next in res) {
                            break
                        }
                        cur = next
                    }
                    res.add(cur)
                }
            }

            Dir.W -> {
                start.sortedBy { it.x }.forEach {
                    var cur = it
                    while (cur.x > 0) {
                        val next = cur.left()
                        if (next in rocks || next in res) {
                            break
                        }
                        cur = next
                    }
                    res.add(cur)
                }
            }

            Dir.S -> {
                start.sortedBy { -it.y }.forEach {
                    var cur = it
                    while (cur.y < sizeY - 1) {
                        val next = cur.down()
                        if (next in rocks || next in res) {
                            break
                        }
                        cur = next
                    }
                    res.add(cur)
                }
            }

            Dir.E -> {
                start.sortedBy { -it.x }.forEach {
                    var cur = it
                    while (cur.x < sizeX - 1) {
                        val next = cur.right()
                        if (next in rocks || next in res) {
                            break
                        }
                        cur = next
                    }
                    res.add(cur)
                }
            }
        }
        return res.toSet()
    }

    enum class Dir {
        N,
        W,
        S,
        E
    }
}

