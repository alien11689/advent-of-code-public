package dpr.aoc2018

object Day18 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/18/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        return solve(input, 10)
    }

    private fun part2(input: List<String>): Any {
        return solve(input, 1000000000)
    }

    private fun solve(input: List<String>, limit: Int): Int {
        val mem = mutableMapOf<List<List<Char>>, Int>()
        var increased = false
        var board = input.map { it.toList() }
        var tick = 0
        while (tick < limit) {
            ++tick
            val newBoard = board.map { it.toMutableList() }.toMutableList()
            for (y in board.indices) {
                for (x in board[0].indices) {
                    val adjacent = neighbours(x, y, board).map { board[it.second][it.first] }
                    if (board[y][x] == '.') {
                        if (adjacent.count { it == '|' } >= 3) {
                            newBoard[y][x] = '|'
                        }
                    } else if (board[y][x] == '|') {
                        if (adjacent.count { it == '#' } >= 3) {
                            newBoard[y][x] = '#'
                        }
                    } else {
                        if (adjacent.count { it == '#' } >= 1 && adjacent.count { it == '|' } >= 1) {
                            // nothing
                        } else {
                            newBoard[y][x] = '.'
                        }
                    }
                }
            }
            board = newBoard
            if (!increased) {
                if (board in mem) {
                    val step = tick - mem[board]!!
                    //                    println("Previosly in tick ${mem[board]}")
                    while (tick + step < limit) {
                        tick += step
                    }
                    //                    println("Move to tick $tick")
                    increased = true
                }
                mem[board] = tick
            }
        }

        val flat = board.flatten()
        val woods = flat.count { it == '|' }
        val lumb = flat.count { it == '#' }
        return lumb * woods
    }

    private fun neighbours(x: Int, y: Int, board: List<List<Char>>): List<Pair<Int, Int>> {
        return listOf(
            x - 1 to y,
            x + 1 to y,
            x to y - 1,
            x to y + 1,
            x - 1 to y - 1,
            x - 1 to y + 1,
            x + 1 to y + 1,
            x + 1 to y - 1,
        ).filter { it.first >= 0 && it.first < board[0].size && it.second >= 0 && it.second < board.size }
    }
}
