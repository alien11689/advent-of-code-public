package dpr.aoc2020

import dpr.commons.Util

object Day05 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/05/input.txt")
        val seats = input.map { Seat.from(it) }
        println(part1(seats))
        println(part2(seats))
    }

    private fun part1(input: List<Seat>): Any {
        return input.maxOf { it.toId() }
    }

    private fun part2(input: List<Seat>): Any {
        val sortedSeats = input.sortedBy { it.toId() }
        var previous = sortedSeats.first()
        sortedSeats.forEach {
            if (it != previous) {
                if (it == previous.next()) {
                    previous = it
                } else {
                    return it.toId() - 1
                }
            }
        }
        throw RuntimeException("No solution")
    }

    data class Seat(val row: Int, val column: Int) {
        fun toId(): Int = row * 8 + column
        fun next(): Seat {
            return from(toId() + 1)
        }

        companion object {
            fun from(s: String): Seat {
                val num = s.fold(0) { acc, c ->
                    acc * 2 + if (c in setOf('F', 'L')) 0 else 1
                }
                return from(num)
            }

            fun from(id: Int): Seat = Seat(id / 8, id % 8)
        }
    }

}

