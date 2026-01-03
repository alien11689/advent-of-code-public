package dpr.aoc2021

import dpr.commons.Util

object Day04 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getLinesFromFile("/04/input.txt")
        val input = readInput(lines)
        println(part1(input))
        println(part2(input))
    }

    data class Input(val boards: List<Board>, val numbers: List<Int>)

    data class Board(val numbers: List<List<Int>>) {
        fun toExtendedBoard(): ExtendedBoard {
            return ExtendedBoard(this, numbers.map { numbers.map { false }.toMutableList() }.toMutableList())
        }
    }

    data class ExtendedBoard(val board: Board, val marked: MutableList<MutableList<Boolean>>) {
        fun mark(n: Int) {
            for (i in board.numbers.indices) {
                for (j in board.numbers.indices) {
                    if (board.numbers[i][j] == n) {
                        marked[i][j] = true
                    }
                }
            }
        }

        fun hasLine(): Boolean {
            for (i in board.numbers.indices) {
                if (marked[i].all { it }) {
                    return true
                }
            }
            for (j in board.numbers[0].indices) {
                if (board.numbers.indices.all { marked[it][j] }) {
                    return true
                }
            }
            return false
        }

        fun result(): Int {
            var sum = 0
            for (i in marked.indices) {
                for (j in marked[i].indices) {
                    if (!marked[i][j]) {
                        sum += board.numbers[i][j]
                    }
                }
            }
            return sum
        }
    }

    @JvmStatic
    fun part1(input: Input): Int {
        val extendedBoards = input.boards.map { it.toExtendedBoard() }
        for (n in input.numbers) {
            for (b in extendedBoards) {
                b.mark(n)
                if (b.hasLine()) {
                    return n * b.result()
                }
            }
        }
        return -1
    }

    @JvmStatic
    fun part2(input: Input): Int {
        val extendedBoards = input.boards.map { it.toExtendedBoard() }
        val winningBoards = mutableListOf<ExtendedBoard>()
        for (n in input.numbers) {
            for (b in extendedBoards) {
                if (b.hasLine()) {
                    continue
                }
                b.mark(n)
                if (b.hasLine()) {
                    winningBoards.add(b)
                }
            }
            if (winningBoards.size == extendedBoards.size) {
                return n * winningBoards.last().result()
            }
        }
        return -1
    }

    @JvmStatic
    fun readInput(lines: List<String>): Input {
        var numbers = emptyList<Int>()
        val boards = mutableListOf<Board>()
        var board = mutableListOf<List<Int>>()
        for (line in lines) {
            if (numbers.isEmpty()) {
                numbers = line.split(",").map { it.toInt() }
            } else if (line.isBlank() || line.isEmpty()) {
                if (board.isNotEmpty()) {
                    boards.add(Board(board))
                }
                board = mutableListOf()
            } else {
                board.add(line.trim().split("\\s+".toRegex()).map { it.toInt() })
            }
        }
        return Input(boards, numbers)
    }
}

