package dpr.aoc2016

import dpr.commons.Util

object Day15 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        println(
            part1And2(
                listOf(
                    Disk(17, 1, 1),
                    Disk(7, 0, 2),
                    Disk(19, 2, 3),
                    Disk(5, 0, 4),
                    Disk(3, 0, 5),
                    Disk(13, 5, 6)
                )
            )
        )
        println(
            part1And2(
                listOf(
                    Disk(17, 1, 1),
                    Disk(7, 0, 2),
                    Disk(19, 2, 3),
                    Disk(5, 0, 4),
                    Disk(3, 0, 5),
                    Disk(13, 5, 6),
                    Disk(11, 0, 7),
                )
            )
        )
    }

    private fun part1And2(input: List<Disk>): Any {
        val biggest = input.maxByOrNull { it.positions }!!
        var time = biggest.first()
        val step = biggest.positions
        while (true) {
            if (input.all { it.at0(time) }) {
                return time
            }
            time += step
        }
    }

    data class Disk(val positions: Int, val start: Int, val num: Int) {
        fun first(): Int = positions - (start + num) % positions
        fun at0(time: Int): Boolean {
            return (start + time + num) % positions == 0
        }
    }
}
