package pl.touk.dpr.aoc2022

object Day17 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/17/input.txt")
        println("Part 1:")
        println(part1(Util.getNotEmptyLinesFromFile("/17/test1.txt")))
        println(part1(lines))
        println("Part 2:")
        println(part2(Util.getNotEmptyLinesFromFile("/17/test1.txt")))
        println(part2(lines))
    }

    data class Point(val x: Int, val y: Int)

    data class Brick(val elems: List<Point>) {
        fun lift(tallest: Int): Brick = copy(elems = elems.map { it.copy(y = it.y + tallest) })
        fun move(side: Char): Brick {
            val dx = when (side) {
                '<' -> -1
                '>' -> 1
                else -> throw RuntimeException()
            }
            return copy(elems = elems.map { it.copy(x = it.x + dx) })
        }

        fun isValid(board: Set<Point>): Boolean {
            return elems.all { it.x in 0 until 7 && it !in board }
        }

        fun down(): Brick = copy(elems = elems.map { it.copy(y = it.y - 1) })
    }

    private fun part1(lines: List<String>): Any {
        val moves = lines[0].toCharArray()
        val board = mutableSetOf<Point>()
        (0 until 7).forEach { board.add(Point(it, 0)) }
        val bricks = mapOf<Int, Brick>(
            0 to Brick(listOf(Point(2, 0), Point(3, 0), Point(4, 0), Point(5, 0))),
            1 to Brick(listOf(Point(3, 0), Point(2, 1), Point(3, 1), Point(4, 1), Point(3, 2))),
            2 to Brick(listOf(Point(2, 0), Point(3, 0), Point(4, 0), Point(4, 1), Point(4, 2))),
            3 to Brick(listOf(Point(2, 0), Point(2, 1), Point(2, 2), Point(2, 3))),
            4 to Brick(listOf(Point(2, 0), Point(3, 0), Point(2, 1), Point(3, 1))),
        )
        var moveIdx = 0
        val limit = 2022
        (0 until limit).forEach { turn ->
//            println("Running turn $turn")
            val tallest = board.maxOf { it.y }
            var brick = bricks[turn % bricks.size]!!.lift(tallest + 4)
//            println("Current brick $brick")
            while (true) {
                val side = moves[moveIdx % moves.size]
                moveIdx++
                val newBrick = brick.move(side)
                if (newBrick.isValid(board)) {
                    brick = newBrick
//                    println("Brick goes $side -> $brick")
                }
                val brickDown = brick.down()
                if (brickDown.isValid(board)) {
                    brick = brickDown
//                    println("Brick goes down -> $brick")
                } else {
                    board.addAll(brick.elems)
//                    println("Brick stable -> $brick")
                    break
                }
            }
        }
//        for (y in board.maxOf { it.y } downTo 0) {
//            for (x in 0 until 7) {
//                if (Point(x, y) in board) print("#") else print('.')
//            }
//            println()
//        }
        return board.maxOf { it.y }
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

