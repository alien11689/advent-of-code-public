package pl.touk.dpr.aoc2019

import pl.touk.dpr.aoc2019.intcode.IntCodeComputer.program
import pl.touk.dpr.aoc2019.intcode.IntCodeComputerState
import java.util.PriorityQueue
import java.util.Stack

object Day15 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/15/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val board = createBoard(input)
        val dest = board.toList().find { it.second == 2L }!!.first

        val visited = mutableSetOf(0L to 0L)
        val pq = initPq()
        pq.offer(listOf(0L, 0L, 0L))
        while (!pq.isEmpty()) {
            val currentState = pq.poll()
            if (currentState[1] == dest.first && currentState[2] == dest.second) {
                return currentState[0]
            }
            generateNewStates(visited, currentState, board, pq)
        }
        throw RuntimeException()
    }

    private fun part2(input: String): Any {
        val board = createBoard(input)
        val dest = board.toList().find { it.second == 2L }!!.first

        val visited = mutableSetOf(0L to 0L)
        val pq = initPq()

        var minutes = -1L

        pq.offer(listOf(0L, dest.first, dest.second))
        while (!pq.isEmpty()) {
            val c = pq.poll()
            minutes = c[0]
            generateNewStates(visited, c, board, pq)
        }
        return minutes
    }

    private fun generateNewStates(visited: MutableSet<Pair<Long, Long>>, currentState: List<Long>, board: Map<Pair<Long, Long>, Long>, pq: PriorityQueue<List<Long>>) {
        visited.add(currentState[1] to currentState[2])
        val nexts = listOf(1, 2, 3, 4).map { nextPos(currentState[1] to currentState[2], it) }.filter {
            it !in visited && it in board.keys && board[it] != 0L
        }
        nexts.forEach {
            pq.offer(listOf(currentState[0] + 1, it.first, it.second))
        }
    }

    private fun createBoard(input: String): Map<Pair<Long, Long>, Long> {
        val state = IntCodeComputerState.init(input)
        val output = state.output
        val inputQ = state.input
        val board = mutableMapOf<Pair<Long, Long>, Long>()
        var cur = 0L to 0L
        board[cur] = 1L
        val path = Stack<Int>()

        while (true) {
            val nextMove = listOf(1, 3, 2, 4).map { nextPos(cur, it) to it }.find {
                it.first !in board.keys
            }
            if (nextMove == null && path.empty()) {
                break
            }
            if (nextMove == null) {
                val prev = path.pop()
                val op = opposite(prev)
                inputQ.offer(op.toLong())
                program(state, output)
                output.poll()
                cur = nextPos(cur, op)
            } else {
                inputQ.offer(nextMove.second.toLong())
                program(state, output)
                val out = output.poll()
                board[nextMove.first] = out
                if (out != 0L) {
                    cur = nextMove.first
                    path.push(nextMove.second)
                }
            }
        }
        return board
    }

    private fun initPq() = PriorityQueue<List<Long>> { a, b ->
        if (a[0] == b[0]) {
            if (a[1] == b[1]) {
                (a[2] - b[2]).toInt()
            } else {
                (a[1] - b[1]).toInt()
            }
        } else {
            (a[0] - b[0]).toInt()
        }
    }

//    fun printBoard(panel: MutableMap<Pair<Long, Long>, Long>) {
//        for (i in (panel.keys.minByOrNull { it.second }!!.second..panel.keys.maxByOrNull { it.second }!!.second)) {
//            for (j in (panel.keys.minByOrNull { it.first }!!.first..panel.keys.maxByOrNull { it.first }!!.first)) {
//                if (i == 0L && j == 0L) {
//                    print('S')
//                } else {
//                    val v = panel[j to i]?.toInt()
//                    if (v == null) {
//                        print(' ')
//                    } else if (v == 0) {
//                        print('#')
//                    } else if (v == 1) {
//                        print('.')
//                    } else if (v == 2) {
//                        print('O')
//                    }
//                }
//            }
//            println()
//        }
//    }

    private fun nextPos(curPos: Pair<Long, Long>, dir: Int): Pair<Long, Long> {
        return when (dir) {
            1 -> Pair(curPos.first, curPos.second - 1)
            2 -> Pair(curPos.first, curPos.second + 1)
            3 -> Pair(curPos.first - 1, curPos.second)
            4 -> Pair(curPos.first + 1, curPos.second)
            else -> throw RuntimeException()
        }
    }

    private fun opposite(dir: Int): Int {
        return when (dir) {
            1 -> 2
            2 -> 1
            3 -> 4
            4 -> 3
            else -> throw RuntimeException()
        }
    }
}
