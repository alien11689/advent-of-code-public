package pl.touk.dpr.aoc2022

object Day17 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/17/input.txt")
        println("Part 1:")
        val part1TurnLimit = 2022L
        println(part1And2(Util.getNotEmptyLinesFromFile("/17/test1.txt"), part1TurnLimit, false))
        println(part1And2(lines, part1TurnLimit, false))
        println("Part 2:")
        val part2TurnLimit = 1000000000000L
        println(part1And2(Util.getNotEmptyLinesFromFile("/17/test1.txt"), part2TurnLimit, false))
        println(part1And2(lines, part2TurnLimit, print = false))
    }

    data class Point(val x: Int, val y: Long)

    data class Brick(val elems: List<Point>) {
        fun lift(tallest: Long): Brick = copy(elems = elems.map { it.copy(y = it.y + tallest) })
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

    private fun part1And2(lines: List<String>, limit: Long, print: Boolean = false): Any {
        val moves = lines[0].toCharArray()
//        println("ALl moves: ${moves.size}")
        val board = mutableMapOf<Point, Pair<Int, Long>>()
        (0 until 7).forEach { board[Point(it, 0)] = 9 to -1 }
        val bricks = mapOf<Int, Brick>(
            0 to Brick(listOf(Point(2, 0), Point(3, 0), Point(4, 0), Point(5, 0))),
            1 to Brick(listOf(Point(3, 0), Point(2, 1), Point(3, 1), Point(4, 1), Point(3, 2))),
            2 to Brick(listOf(Point(2, 0), Point(3, 0), Point(4, 0), Point(4, 1), Point(4, 2))),
            3 to Brick(listOf(Point(2, 0), Point(2, 1), Point(2, 2), Point(2, 3))),
            4 to Brick(listOf(Point(2, 0), Point(3, 0), Point(2, 1), Point(3, 1))),
        )
        var moveIdx = 0
        var turn = 0L
        val turns = mutableMapOf<Long, Pair<Long, Int>>() // turn to (tall to move)
        var tallBoost: Long = 0
        val possibleMasks = mutableMapOf<Map<Point, Int>, Pair<Long, Long>>() // mask to (firstSeenMask, tallAtFirstSeenMask)
        var maskUsed = false
        while (turn < limit) {
//            println("Running turn $turn")
            val brickIdx = (turn % bricks.size).toInt()
            val tallest = board.keys.maxOf { it.y }
            if (!maskUsed && brickIdx == 0 && turn > 20) {
                // Mask can be found by printing the result after some big number of rounds but not so big for sequential calculations e.g. 10000
                // and finding two turns where only items from those two rounds are used
                // so it's done manually (in previous commits) when look e.g. for base line .10000.
                // remember to generate some turns more to see if previous rounds could be pivotal
                val turnsToCheck = (turn - 15)..(turn - 6)
                val possibleMask = board.filter { it.value.second in turnsToCheck }
                val ys = possibleMask.map { it.key.y }.toSet()
                if (board.filter { it.key.y in ys }.all { it.value.second in turnsToCheck }) {
                    val firstSeenMask = turn - 5
                    val tallAtFirstSeenMask = turns[turn - 6]!!.first
                    val mask = possibleMask.map { it.key.copy(y = it.key.y - tallAtFirstSeenMask) to it.value.first }.toMap()
                    possibleMasks[mask] = firstSeenMask to tallAtFirstSeenMask
//                    println("Found possible mask: $possibleMask being in $firstSeenMask")
                }
            }
            if (!maskUsed && possibleMasks.isNotEmpty() && brickIdx == 0) {
                for ((mask, res) in possibleMasks) {
                    val instantMask = mask.mapKeys { it.key.copy(y = tallest + it.key.y) }
//                println(instantMask)
                    if (instantMask.all { maskElem -> board[maskElem.key]?.first == maskElem.value }) {
//                        println("Mask repeated at $turn - $mask")
//                    println("Found mask at $turn, cur $moveIdx tallest is $tallest")
                        val step = turn - res.first
                        val dy = tallest - res.second
                        val occurencesTillLimit = (limit - turn) / step
                        turn += occurencesTillLimit * step
                        tallBoost += occurencesTillLimit * dy
                        maskUsed = true
//                        println("Moving to turn $turn with $tallBoost")
                        break
                    }
                }
            }
            var brick = bricks[brickIdx]!!.lift(tallest + 4)
            while (true) {
                val side = moves[moveIdx % moves.size]
                moveIdx++
                val newBrick = brick.move(side)
                if (newBrick.isValid(board.keys)) {
                    brick = newBrick
                }
                val brickDown = brick.down()
                if (brickDown.isValid(board.keys)) {
                    brick = brickDown
                } else {
                    board.putAll(brick.elems.map { it to (brickIdx to turn) })
                    turns[turn] = brick.elems.maxOf { it.y } to moveIdx
                    ++turn
                    break
                }
            }
        }
        if (print) {
            for (y in board.keys.maxOf { it.y } downTo 0) {
                for (x in 0 until 7) {
                    print(board[Point(x, y)]?.first ?: '.')
                }
                println()
            }
        }
        return board.keys.maxOf { it.y } + tallBoost
    }

}

