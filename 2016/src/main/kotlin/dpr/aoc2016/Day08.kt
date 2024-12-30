package dpr.aoc2016

import dpr.commons.Util

object Day08 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/08/input.txt")
        val (part1, part2) = part1And2(input)
        println(part1)
        println(part2)
    }

    private fun part1And2(input: List<String>): Pair<Any, Any> {
        val board = (0..5).map {
            (0..49).map { false }
        }
        val res = input.fold(board) { acc, command ->
            val parts = command.split(Regex("[ xy=]+"))
            when (parts[0]) {
                "rect" -> List(acc.size) { y ->
                    acc[y].mapIndexed { x, cell ->
                        if (x < parts[1].toInt() && y < parts[2].toInt()) {
                            true
                        } else {
                            cell
                        }
                    }
                }

                "rotate" -> {
                    val which = parts[2].toInt()
                    val by = parts[4].toInt()
                    when (parts[1]) {
                        "column" -> List(acc.size) { y ->
                            acc[y].mapIndexed { x, cell ->
                                if (x == which) {
                                    acc[(acc.size + y - by) % acc.size][x]
                                } else {
                                    cell
                                }
                            }
                        }

                        "row" -> List(acc.size) { y ->
                            acc[y].mapIndexed { x, cell ->
                                if (y == which) {
                                    acc[y][(acc[y].size + x - by) % acc[y].size]
                                } else {
                                    cell
                                }
                            }
                        }

                        else -> acc
                    }
                }

                else -> acc
            }
        }
        val part1 = res.flatten().count { it }
        val part2 = StringBuilder()
        (0..5).forEach { y ->
            (0..49).forEach { x ->
                part2.append(if (res[y][x]) '#' else ' ')
            }
            if (y < 5) {
                part2.append("\n")
            }
        }
        return Pair(part1, part2.toString())
    }
}
