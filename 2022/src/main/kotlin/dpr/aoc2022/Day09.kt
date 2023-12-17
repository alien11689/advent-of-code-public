package dpr.aoc2022

import dpr.commons.Util

object Day09 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/09/input.txt")
//        println("Part 1:")
//        println(part1(Util.getNotEmptyLinesFromFile("/09/test1.txt")))
        println(part1(lines))
//        println("Part 2:")
//        println(part2(Util.getNotEmptyLinesFromFile("/09/test1.txt")))
//        println(part2(Util.getNotEmptyLinesFromFile("/09/test2.txt")))
        println(part2(lines))
        // 2642 is wrong
    }

    private fun part1(lines: List<String>): Any {
        val state = State(List(2) { Point(0, 0) })
        val tailVisited = traverse(state, lines)
        return tailVisited.size
    }

    data class Point(val x: Int, val y: Int) {
        fun move(dir: String) = when (dir) {
            "R" -> copy(x = x + 1)
            "L" -> copy(x = x - 1)
            "U" -> copy(y = y + 1)
            "D" -> copy(y = y - 1)
            else -> throw RuntimeException("unknown $dir")
        }

        fun moveToward(newHead: Point): Point {
            val newHeadNeighbours = newHead.neighbours()
            return if (newHead.x == x || newHead.y == y) {
                //                println("Moving $this towards $newHead, possible moves lines: ${neighboursLines()}")
                neighboursLines().first { it in newHeadNeighbours }
            } else {
                //                println("Moving $this towards $newHead, possible moves diagonal: ${neighboursDiagonal()}")
                neighboursDiagonal().first { it in newHeadNeighbours }
            }
        }

        fun neighbours(): Set<Point> = neighboursDiagonal() + neighboursLines() + this

        private fun neighboursLines(): Set<Point> = setOf(
            copy(y = y + 1),
            copy(y = y - 1),
            copy(x = x + 1),
            copy(x = x - 1),
        )

        private fun neighboursDiagonal(): Set<Point> = setOf(
            copy(x = x - 1, y = y + 1),
            copy(x = x - 1, y = y - 1),
            copy(x = x + 1, y = y + 1),
            copy(x = x + 1, y = y - 1),
        )
    }

    private fun part2(lines: List<String>): Any {
        val state = State(List(10) { Point(0, 0) })
        val tailVisited = traverse(state, lines)
        return tailVisited.size
    }

    private fun traverse(initState: State, lines: List<String>): MutableSet<Point> {
        var state = initState
        val tailVisited = mutableSetOf(state.points.last())
        lines.forEach {
            //            println("Moving $it")
            val (dir, step) = it.split(" ")
            repeat(step.toInt()) {
                state = state.move(dir)
                tailVisited.add(state.points.last())
                //                println("Head: ${state.head}, tail: ${state.tail}")
            }
            //            draw(state)
        }
        return tailVisited
    }

//    private fun draw(state: State) {
//        for (i in 20 downTo -10) {
//            for (j in -20..20) {
//                if (Point(j, i) in state.points) {
//                    print("#")
//                } else if (j == 0 && i == 0) {
//                    print('s')
//                } else {
//                    print(".")
//                }
//            }
//            println()
//        }
//    }

    data class State(val points: List<Point>) {
        fun move(dir: String): State {
            var newHead = points.first().move(dir)
            val newPoints = mutableListOf(newHead)
            for (i in (1 until points.size)) {
                val cur = points[i]
                val newCur = if (newHead in cur.neighbours()) cur else cur.moveToward(newHead)
                newPoints.add(newCur)
                newHead = newCur
                if (newCur == cur) {
                    newPoints.addAll(points.drop(i + 1))
                    break
                }
            }
            return copy(points = newPoints)
        }
    }
}

