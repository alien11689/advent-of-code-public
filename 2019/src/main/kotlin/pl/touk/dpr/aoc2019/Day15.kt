package pl.touk.dpr.aoc2019

import pl.touk.dpr.aoc2019.intcode.IntCodeComputer
import pl.touk.dpr.aoc2019.intcode.IntCodeComputer.program
import pl.touk.dpr.aoc2019.intcode.IntCodeComputerState
import java.util.LinkedList
import java.util.PriorityQueue
import java.util.Stack
import kotlin.RuntimeException

object Day15 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/15/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val v = IntCodeComputer.parseInput(input)
        val output = LinkedList<Long>()
        val inputQ = LinkedList<Long>()
        val state = IntCodeComputerState(v, input = inputQ)

        val board = mutableMapOf<Pair<Long, Long>, Long>()

        var cur = 0L to 0L
        board[cur] = 1L
        val path = Stack<Int>()

        while (true) {
            val nextMove = listOf(1, 3, 2, 4).map { nextPos(cur, it) to it }.find {
                !(it.first in board.keys)
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

        cur = 0L to 0L

        val dest = board.toList().find { it.second == 2L }!!.first

        val visited = mutableSetOf(cur)
        val pq = PriorityQueue<List<Long>> { a, b ->
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
        pq.offer(listOf(0L, 0L, 0L))
        while (!pq.isEmpty()) {
            val c = pq.poll()
            if (c[1] == dest.first && c[2] == dest.second) {
                return c[0]
            }
            visited.add(c[1] to c[2])
            val nexts = listOf(1, 2, 3, 4).map { nextPos(c[1] to c[2], it) }.filter {
                it !in visited && it in board.keys && board[it] != 0L
            }
            nexts.forEach {
                pq.offer(listOf(c[0] + 1, it.first, it.second))
            }
        }
        throw RuntimeException()
    }

    private fun part2(input: String): Any {
        val v = IntCodeComputer.parseInput(input)
        val output = LinkedList<Long>()
        val inputQ = LinkedList<Long>()
        val state = IntCodeComputerState(v, input = inputQ)

        val board = mutableMapOf<Pair<Long, Long>, Long>()

        var cur = 0L to 0L
        board[cur] = 1L
        val path = Stack<Int>()

        while (true) {
            val nextMove = listOf(1, 3, 2, 4).map { nextPos(cur, it) to it }.find {
                !(it.first in board.keys)
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

        cur = 0L to 0L

        val dest = board.toList().find { it.second == 2L }!!.first

        val visited = mutableSetOf(cur)
        val pq = PriorityQueue<List<Long>> { a, b ->
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

        var minutes = -1L

        pq.offer(listOf(0L, dest.first, dest.second))
        while (!pq.isEmpty()) {
            val c = pq.poll()
            minutes = c[0]
            visited.add(c[1] to c[2])
            val nexts = listOf(1, 2, 3, 4).map { nextPos(c[1] to c[2], it) }.filter {
                it !in visited && it in board.keys && board[it] != 0L
            }
            nexts.forEach {
                pq.offer(listOf(c[0] + 1, it.first, it.second))
            }
        }
        return minutes
    }

    fun printBoard(panel: MutableMap<Pair<Long, Long>, Long>) {
        for (i in (panel.keys.minByOrNull { it.second }!!.second..panel.keys.maxByOrNull { it.second }!!.second)) {
            for (j in (panel.keys.minByOrNull { it.first }!!.first..panel.keys.maxByOrNull { it.first }!!.first)) {
                if (i == 0L && j == 0L) {
                    print('S')
                } else {
                    val v = panel[j to i]?.toInt()
                    if (v == null) {
                        print(' ')
                    } else if (v == 0) {
                        print('#')
                    } else if (v == 1) {
                        print('.')
                    } else if (v == 2) {
                        print('O')
                    }
                }
            }
            println()
        }
    }

    fun nextPos(curPos: Pair<Long, Long>, dir: Int): Pair<Long, Long> {
        return when (dir) {
            1 -> Pair(curPos.first, curPos.second - 1)
            2 -> Pair(curPos.first, curPos.second + 1)
            3 -> Pair(curPos.first - 1, curPos.second)
            4 -> Pair(curPos.first + 1, curPos.second)
            else -> throw RuntimeException()
        }
    }

    fun opposite(dir: Int): Int {
        return when (dir) {
            1 -> 2
            2 -> 1
            3 -> 4
            4 -> 3
            else -> throw RuntimeException()
        }
    }
}
