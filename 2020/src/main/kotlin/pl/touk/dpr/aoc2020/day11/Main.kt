package pl.touk.dpr.aoc2020.day11

import pl.touk.dpr.aoc2020.Util

private val Pair<Int, Int>.neighbours: Set<Pair<Int, Int>>
    get() {
        return setOf(
                Pair(first + 1, second),
                Pair(first - 1, second),
                Pair(first, second + 1),
                Pair(first, second - 1),
                Pair(first + 1, second + 1),
                Pair(first + 1, second - 1),
                Pair(first - 1, second - 1),
                Pair(first - 1, second + 1)
        )
    }

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getFileContent("/11/input.txt")
//        val input = Util.getFileContent("/11/test.txt")
                .lines()
                .filter { it.isNotEmpty() }
        part1(input)
        part2(input)
    }

    private fun part1(input: List<String>) {
        var seats = readSeats(input)
        while (true) {
            val newSeats = mutableMapOf<Pair<Int, Int>, Boolean>()
            seats.forEach { entry ->
                if (!entry.value && entry.key.neighbours.filter { it in seats.keys }.all { seats[it] == false }) {
                    newSeats[entry.key] = true
                } else if (entry.value && entry.key.neighbours.filter { it in seats.keys }.count { seats[it]!! } >= 4) {
                    newSeats[entry.key] = false
                } else {
                    newSeats[entry.key] = entry.value
                }
            }
            if (seats == newSeats) {
                break
            }
            seats = newSeats
        }
        println(seats.count { it.value })
    }

    private fun readSeats(input: List<String>): MutableMap<Pair<Int, Int>, Boolean> {
        val seats = mutableMapOf<Pair<Int, Int>, Boolean>()
        input.forEachIndexed { i, line ->
            line.forEachIndexed { j, c ->
                if (c == 'L') {
                    seats[Pair(i, j)] = false
                }
            }
        }
        return seats
    }

    private fun part2(input: List<String>) {
        var seats = readSeats(input)
        val maxI = input.size
        val maxJ = input[0].length
        var iter = 0
        while (true) {
            ++iter
            val newSeats = mutableMapOf<Pair<Int, Int>, Boolean>()
            seats.forEach { entry ->
                val neighbours = listOf(
                        findValidOccupied(seats, entry.key, -1, -1, maxI, maxJ),
                        findValidOccupied(seats, entry.key, +1, +1, maxI, maxJ),
                        findValidOccupied(seats, entry.key, -1, +1, maxI, maxJ),
                        findValidOccupied(seats, entry.key, +1, -1, maxI, maxJ),
                        findValidOccupied(seats, entry.key, -1, 0, maxI, maxJ),
                        findValidOccupied(seats, entry.key, +1, 0, maxI, maxJ),
                        findValidOccupied(seats, entry.key, 0, -1, maxI, maxJ),
                        findValidOccupied(seats, entry.key, 0, +1, maxI, maxJ),
                ).count { it }
                if (!entry.value && neighbours == 0) {
                    newSeats[entry.key] = true
                } else if (entry.value && neighbours >= 5) {
                    newSeats[entry.key] = false
                } else {
                    newSeats[entry.key] = entry.value
                }
            }
            if (seats == newSeats) {
                break
            }
            seats = newSeats
//            println(iter)
//            (0..(maxI - 1)).forEach { i ->
//                (0..(maxJ - 1)).forEach { j ->
//                    when(seats[Pair(i,j)]){
//                        true -> print('#')
//                        false -> print('L')
//                        else -> print('.')
//                    }
//                }
//                println()
//            }
        }
        println(seats.count { it.value })

    }

    private fun findValidOccupied(seats: MutableMap<Pair<Int, Int>, Boolean>, current: Pair<Int, Int>, changeI: Int, changeJ: Int, maxI: Int, maxJ: Int): Boolean {
        var cur = current
        while (true) {
            val next = Pair(cur.first + changeI, cur.second + changeJ)
            if (!next.isValid(maxI, maxJ)) {
                return false
            }
            if (seats[next] == true) {
                return true
            }
            if (seats[next] == false) {
                return false
            }
            cur = next
        }
    }
}

private fun Pair<Int, Int>.isValid(maxI: Int, maxJ: Int): Boolean {
    return first >= 0 && second >= 0 && first < maxI && second < maxJ
}
