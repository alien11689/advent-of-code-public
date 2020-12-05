package pl.touk.dpr.aoc2020.day05

import pl.touk.dpr.aoc2020.Util

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getFileContent("/05/input.txt")
        val seats = input.lines()
                .filter { it.isNotEmpty() }
                .map { Seat.from(it) }
        part1(seats)
        part2(seats)
    }

    private fun part1(input: List<Seat>) {
        println(input.map { it.toId() }.maxOrNull()!!)
    }

    private fun part2(input: List<Seat>) {
        val sortedSeats = input.sortedBy { it.toId() }
        var previous = sortedSeats.first()
        sortedSeats.forEach {
            if (it != previous) {
                if (it == previous.next()) {
                    previous = it
                } else {
                    println(it.toId() - 1)
                    return
                }
            }
        }
    }

    data class Seat(val row: Int, val column: Int) {
        fun toId(): Int = row * 8 + column
        fun next(): Seat {
            return from(toId() + 1)
        }

        companion object {
            fun from(s: String): Seat {
                val row = s.subSequence(0, s.length - 3).fold(0) { acc, c ->
                    acc * 2 + if (c == 'F') 0 else 1
                }
                val col = s.subSequence(s.length - 3, s.length).fold(0) { acc, c ->
                    acc * 2 + if (c == 'L') 0 else 1
                }
                return Seat(row, col)
            }

            fun from(id: Int): Seat = Seat(id / 8, id % 8)
        }
    }

}

