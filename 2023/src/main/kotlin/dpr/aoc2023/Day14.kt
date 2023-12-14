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
        val rocks = mutableSetOf<Point2D>()
        val movableRocks = mutableSetOf<Point2D>()
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                when (c) {
                    '#' -> rocks.add(Point2D(x, y))
                    '.' -> {}
                    'O' -> movableRocks.add(Point2D(x, y))
                }
            }
        }
        val memory = mutableMapOf<Set<Point2D>, Int>()
        var current = movableRocks.toSet()
        memory[current] = 0
        var i = 0
        while (i < 1000000000) {
            ++i
            current = cycle(current, rocks, lines[0].length, lines.size)
            if (current in memory) {
                val prev = memory[current]!!
                val diff = i - prev
//                throw RuntimeException("After cycle $i is the same as $prev (diff $diff)")
                while (i + diff < 1000000000) {
                    i += diff
                }
                println("Current i = $i")
                memory.clear()
            }
            memory[current] = i
        }
        return current.sumOf { lines.size - it.y }
    }

    enum class Dir {
        N,
        W,
        S,
        E
    }

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

            else -> throw RuntimeException()
        }
        return res.toSet()
    }
}

