package pl.touk.dpr.aoc2022

object Day17 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/17/input.txt")
        println("Part 1:")
//        println(part1(Util.getNotEmptyLinesFromFile("/17/test1.txt")))
        println(part1(lines))
        println("Part 2:")
        val part2TurnLimit = 1000000000000L
        // Mask can be found by printing the result after some big number of rounds but not so big for sequential calculations e.g. 10000
        // and finding two turns where only items from those two rounds are used
        // so it's done manually when look e.g. for base line .10000.
//        val exampleMaskRaw = """
//            ..442..
//            ..442..
//            ..2221.
//            ....111
//            .....1.
//            .30000.
//            .3..3..
//            .3..3..
//            .3443.2
//            .1443.2
//            111.222
//            .10000.
//        """.trimIndent()
//        println(part2(Util.getNotEmptyLinesFromFile("/17/test1.txt"), part2TurnLimit, exampleMaskRaw, false))
        val inputMaskRaw = """
            ...3...
            ..23...
            ..23...
            2223...
            0000...
            ..3....
            ..3....
            ..3.44.
            ..3.44.
            ..2..1.
            ..2.111
            222..1.
            .1...44
            111..44
            .10000.
        """.trimIndent()
        println(part2(lines, part2TurnLimit, inputMaskRaw, print = false))
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

    private fun part2(lines: List<String>, limit: Long, maskRaw: String, print: Boolean = false): Any {
        val moves = lines[0].toCharArray()
//        println("ALl moves: ${moves.size}")
        val board = mutableMapOf<Point, Int>()
        (0 until 7).forEach { board[Point(it, 0)] = 9 }
        val bricks = mapOf<Int, Brick>(
            0 to Brick(listOf(Point(2, 0), Point(3, 0), Point(4, 0), Point(5, 0))),
            1 to Brick(listOf(Point(3, 0), Point(2, 1), Point(3, 1), Point(4, 1), Point(3, 2))),
            2 to Brick(listOf(Point(2, 0), Point(3, 0), Point(4, 0), Point(4, 1), Point(4, 2))),
            3 to Brick(listOf(Point(2, 0), Point(2, 1), Point(2, 2), Point(2, 3))),
            4 to Brick(listOf(Point(2, 0), Point(3, 0), Point(2, 1), Point(3, 1))),
        )
        //Good masK after two rounds found by printing
        val mask = maskRaw.lines().flatMapIndexed { y, line ->
            line.mapIndexed { x, c ->
                when (c) {
                    in '0'..'4' -> Point(x, -y) to c.toString().toInt()
                    else -> null
                }
            }.filterNotNull()
        }.toMap()
//        println(mask)

        var moveIdx = 0
        var turn = 0L
        var firstSeenMask: Long? = null
        var tallAtFirstSeenMask: Long? = null
        var tallBoost: Long = 0
        while (turn < limit) {
            val brickIdx = (turn % bricks.size).toInt()
            val tallest = board.keys.maxOf { it.y }
            if (brickIdx == 0) {
                val instantMask = mask.mapKeys { it.key.copy(y = tallest + it.key.y) }
//                println(instantMask)
                if (instantMask.all { maskElem -> board[maskElem.key] == maskElem.value }) {
//                    println("Found mask at $turn, cur $moveIdx tallest is $tallest")
                    if (firstSeenMask == null) {
                        firstSeenMask = turn
                        tallAtFirstSeenMask = tallest.toLong()
                    } else {
                        val step = turn - firstSeenMask
                        val dy = tallest.toLong() - tallAtFirstSeenMask!!
                        while (turn + step < limit) {
                            turn += step
                            tallBoost += dy
                        }
                    }
                }
            }
//            println("Running turn $turn")
            var brick = bricks[brickIdx]!!.lift(tallest + 4)
//            println("Current brick $brick")
            while (true) {
                val side = moves[moveIdx % moves.size]
                moveIdx++
                val newBrick = brick.move(side)
                if (newBrick.isValid(board.keys)) {
                    brick = newBrick
//                    println("Brick goes $side -> $brick")
                }
                val brickDown = brick.down()
                if (brickDown.isValid(board.keys)) {
                    brick = brickDown
//                    println("Brick goes down -> $brick")
                } else {
                    board.putAll(brick.elems.map { it to brickIdx })
                    if (brickIdx == 0) {
//                        println("Laying pivot $brick")
                    }
//                    println("Brick stable -> $brick")
                    ++turn
                    break
                }
            }
        }
        if (print) {
            for (y in board.keys.maxOf { it.y } downTo 0) {
                for (x in 0 until 7) {
                    print(board[Point(x, y)] ?: '.')
                }
                println()
            }
        }
        return board.keys.maxOf { it.y } + tallBoost
    }
}

