package dpr.aoc2016

object Day08 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/08/input.txt")
        part1And2(input)
    }

    private fun part1And2(input: List<String>) {
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
        println(res.flatten().count { it })
        (0..5).forEach { y ->
            (0..49).forEach { x ->
                print(if (res[y][x]) '#' else ' ')
            }
            println()
        }
    }
}
