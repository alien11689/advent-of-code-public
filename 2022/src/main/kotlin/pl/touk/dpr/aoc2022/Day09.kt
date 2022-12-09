package pl.touk.dpr.aoc2022

object Day09 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/09/input.txt")
        println("Part 1:")
//        println(part1(Util.getNotEmptyLinesFromFile("/09/test1.txt")))
        println(part1(lines))
        println("Part 2:")
//        println(part2(Util.getNotEmptyLinesFromFile("/09/test1.txt")))
//        println(part2(Util.getNotEmptyLinesFromFile("/09/test2.txt")))
        println(part2(lines))
        // 2642 is wrong
    }

    private fun part1(lines: List<String>): Any {
        var state = State(Point(0, 0), Point(0, 0))
//        println("Head: ${state.head}, tail: ${state.tail}")
        lines.forEach {
//            println("Moving $it")
            val (dir, step) = it.split(" ")
            (1..(step.toInt())).forEach {
                state = state.move(dir)
//                println("Head: ${state.head}, tail: ${state.tail}")
            }
        }
        return state.tailVisited.size
    }

    data class Point(val x: Int, val y: Int) {
        fun move(dir: String) = when (dir) {
            "R" -> copy(x + 1, y)
            "L" -> copy(x - 1, y)
            "U" -> copy(x, y + 1)
            "D" -> copy(x, y - 1)
            else -> throw RuntimeException("unknown $dir")
        }

        fun moveToward(newHead: Point): Point {
            if (newHead.x == x || newHead.y == y) {
//                println("Moving $this towards $newHead, possible moves lines: ${neighboursLines()}")
                return newHead.neighbours().intersect(this.neighboursLines()).first()
            } else {
//                println("Moving $this towards $newHead, possible moves diagonal: ${neighboursDiagonal()}")
                return newHead.neighbours().intersect(this.neighboursDiagonal()).first()
            }
        }

        fun neighbours(): Set<Point> = setOf(this) + neighboursDiagonal() + neighboursLines()

        fun neighboursLines(): Set<Point> = setOf(
            copy(x = x, y = y + 1),
            copy(x = x, y = y - 1),
            copy(x = x + 1, y = y),
            copy(x = x - 1, y = y),
        )

        fun neighboursDiagonal(): Set<Point> = setOf(
            copy(x = x - 1, y = y + 1),
            copy(x = x - 1, y = y - 1),
            copy(x = x + 1, y = y + 1),
            copy(x = x + 1, y = y - 1),
        )
    }

    data class State(val head: Point, val tail: Point, val tailVisited: Set<Point> = setOf(tail)) {
        fun move(dir: String): State {
            val newHead = head.move(dir)
            val newTail = if (newHead in tail.neighbours()) tail else {
                tail.moveToward(newHead)
            }
            return copy(head = newHead, tail = newTail, tailVisited = tailVisited + newTail)
        }
    }

    private fun part2(lines: List<String>): Any {
        var state = State2((1..10).map { Point(0, 0) })
//        println("Head: ${state.head}, tail: ${state.tail}")
        lines.forEach {
//            println("Moving $it")
            val (dir, step) = it.split(" ")
            (1..(step.toInt())).forEach {
                state = state.move(dir)
//                println("Head: ${state.head}, tail: ${state.tail}")
            }
//            draw(state)
        }
        return state.tailVisited.size
    }

    private fun draw(state: State2) {
        for (i in 20 downTo -10) {
            for (j in -20..20) {
                if (Point(j, i) in state.points) {
                    print("#")
                } else if (j == 0 && i == 0) {
                    print('s')
                } else {
                    print(".")
                }
            }
            println()
        }
    }

    data class State2(val points: List<Point>, val tailVisited: Set<Point> = setOf(points.last())) {
        fun move(dir: String): State2 {
            var newHead = points.first().move(dir)
            val newPoints = mutableListOf(newHead)
            var moved = true
            for (i in (1 until points.size)) {
                val cur = points[i]
                if (moved) {
                    val newCur = if (newHead in cur.neighbours()) cur else {
                        cur.moveToward(newHead)
                    }
                    newPoints.add(newCur)
                    newHead = newCur
                    if (newCur == cur) {
                        moved = false
                    }
                } else {
                    newPoints.add(cur)
                }
            }
            if (newPoints.size != 10) {
                throw RuntimeException()
            }
//            if (points[8] != newPoints[8]) {
//                println("Moving tail to ${newPoints[8]}")
//            }
            return copy(newPoints, tailVisited = tailVisited + newPoints.last())
        }
    }

}

