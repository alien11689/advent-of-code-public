package pl.touk.dpr.aoc2020.day11

import pl.touk.dpr.aoc2020.Util

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/11/input.txt")
        part1(input)
        part2(input)
    }

    private fun part1(input: List<String>) {
        var seats = readSeats(input)
        while (true) {
            val newSeats = mutableMapOf<Seat, Boolean>()
            seats.forEach { entry ->
                val neighbours = entry.key.neighbours().count { seats[it] == true }
                if (!entry.value && neighbours == 0) {
                    newSeats[entry.key] = true
                } else if (entry.value && neighbours >= 4) {
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

    private fun readSeats(input: List<String>): MutableMap<Seat, Boolean> {
        val seats = mutableMapOf<Seat, Boolean>()
        input.forEachIndexed { i, line ->
            line.forEachIndexed { j, c ->
                if (c == 'L') {
                    seats[Seat(i, j)] = false
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
            val newSeats = mutableMapOf<Seat, Boolean>()
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
        }
        println(seats.count { it.value })

    }

    private fun findValidOccupied(seats: MutableMap<Seat, Boolean>, current: Seat, changeI: Int, changeJ: Int, maxI: Int, maxJ: Int): Boolean {
        var cur = current
        while (true) {
            val next = Seat(cur.i + changeI, cur.j + changeJ)
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

    data class Seat(val i: Int, val j: Int) {
        fun neighbours(): Set<Seat> = setOf(
                Seat(i + 1, j),
                Seat(i - 1, j),
                Seat(i, j + 1),
                Seat(i, j - 1),
                Seat(i + 1, j + 1),
                Seat(i + 1, j - 1),
                Seat(i - 1, j - 1),
                Seat(i - 1, j + 1)
        )

        fun isValid(maxI: Int, maxJ: Int): Boolean {
            return i >= 0 && j >= 0 && i < maxI && j < maxJ
        }
    }
}
