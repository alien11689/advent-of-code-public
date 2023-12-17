package dpr.aoc2017

import dpr.commons.Util

object Day25 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        println(part1())
    }

    private fun part1(): Any {
        var state = 'A'

        val steps = 12656374

        val tape = Tape()

        repeat(steps) {
            when (state) {
                'A' ->
                    state = if (tape.read() == 0) {
                        tape.write(1)
                        tape.right()
                        'B'
                    } else {
                        tape.write(0)
                        tape.left()
                        'C'
                    }

                'B' ->
                    state = if (tape.read() == 0) {
                        tape.write(1)
                        tape.left()
                        'A'
                    } else {
                        tape.write(1)
                        tape.left()
                        'D'
                    }

                'C' ->
                    state = if (tape.read() == 0) {
                        tape.write(1)
                        tape.right()
                        'D'
                    } else {
                        tape.write(0)
                        tape.right()
                        'C'
                    }

                'D' ->
                    state = if (tape.read() == 0) {
                        tape.write(0)
                        tape.left()
                        'B'
                    } else {
                        tape.write(0)
                        tape.right()
                        'E'
                    }

                'E' ->
                    state = if (tape.read() == 0) {
                        tape.write(1)
                        tape.right()
                        'C'
                    } else {
                        tape.write(1)
                        tape.left()
                        'F'
                    }

                'F' ->
                    state = if (tape.read() == 0) {
                        tape.write(1)
                        tape.left()
                        'E'
                    } else {
                        tape.write(1)
                        tape.right()
                        'A'
                    }
            }
        }

        return tape.value()

    }

    class Tape {
        var cur = 0
        var values = mutableMapOf<Int, Int>()

        fun read(): Int {
            return values[cur] ?: 0
        }

        fun write(v: Int) {
            values[cur] = v
        }

        fun left() {
            --cur
        }

        fun right() {
            ++cur
        }

        fun value(): Int {
            return values.values.sum()
        }
    }
}
