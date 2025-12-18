package dpr.aoc2016

import dpr.commons.Util
import dpr.commons.Point2D as Pos

object Day02 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/02/input.txt")
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: List<String>): String {
        val board: Map<Pos, Char> = mapOf(
            Pair(Pos(0, 0), '1'),
            Pair(Pos(1, 0), '2'),
            Pair(Pos(2, 0), '3'),
            Pair(Pos(0, 1), '4'),
            Pair(Pos(1, 1), '5'),
            Pair(Pos(2, 1), '6'),
            Pair(Pos(0, 2), '7'),
            Pair(Pos(1, 2), '8'),
            Pair(Pos(2, 2), '9'),
        )
        val pos = Pos(1, 1)
        return solve(input, pos, board)
    }

    private fun solve(input: List<String>, pos: Pos, board: Map<Pos, Char>): String {
        val res = mutableListOf<Char>()
        input.fold(pos) { oldPos, line ->
            val newPos = traverse(oldPos, line, board)
            res.add(board[newPos]!!)
            newPos
        }
        return res.joinToString(separator = "")
    }

    private fun traverse(init: Pos, line: String, board: Map<Pos, Char>): Pos {
        return line.fold(init) { oldPos, c ->
            when (c) {
                'U' -> if (oldPos.up() in board) oldPos.up() else oldPos
                'L' -> if (oldPos.left() in board) oldPos.left() else oldPos
                'R' -> if (oldPos.right() in board) oldPos.right() else oldPos
                'D' -> if (oldPos.down() in board) oldPos.down() else oldPos
                else -> throw RuntimeException()
            }
        }
    }

    @JvmStatic
    fun part2(input: List<String>): String {
        val board: Map<Pos, Char> = mapOf(
            Pair(Pos(2, 0), '1'),
            Pair(Pos(1, 1), '2'),
            Pair(Pos(2, 1), '3'),
            Pair(Pos(3, 1), '4'),
            Pair(Pos(0, 2), '5'),
            Pair(Pos(1, 2), '6'),
            Pair(Pos(2, 2), '7'),
            Pair(Pos(3, 2), '8'),
            Pair(Pos(4, 2), '9'),
            Pair(Pos(1, 3), 'A'),
            Pair(Pos(2, 3), 'B'),
            Pair(Pos(3, 3), 'C'),
            Pair(Pos(2, 4), 'D'),
        )
        val pos = Pos(0, 2)
        return solve(input, pos, board)
    }
}
