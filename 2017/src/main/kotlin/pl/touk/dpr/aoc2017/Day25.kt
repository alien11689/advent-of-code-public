package pl.touk.dpr.aoc2017

object Day25 {
    @JvmStatic
    fun main(args: Array<String>) {
        println(part1())
    }

    private fun part1(): Any {
        var state = 'A'

        val steps = 12656374

        val tape = Tape()

        (0 until steps).forEach {
            when (state) {
                'A' ->
                    if (tape.read() == 0) {
                        tape.write(1)
                        tape.right()
                        state = 'B'
                    } else {
                        tape.write(0)
                        tape.left()
                        state = 'C'
                    }
                'B' ->
                    if (tape.read() == 0) {
                        tape.write(1)
                        tape.left()
                        state = 'A'
                    } else {
                        tape.write(1)
                        tape.left()
                        state = 'D'
                    }
                'C' ->
                    if (tape.read() == 0) {
                        tape.write(1)
                        tape.right()
                        state = 'D'
                    } else {
                        tape.write(0)
                        tape.right()
                        state = 'C'
                    }
                'D' ->
                    if (tape.read() == 0) {
                        tape.write(0)
                        tape.left()
                        state = 'B'
                    } else {
                        tape.write(0)
                        tape.right()
                        state = 'E'
                    }
                'E' ->
                    if (tape.read() == 0) {
                        tape.write(1)
                        tape.right()
                        state = 'C'
                    } else {
                        tape.write(1)
                        tape.left()
                        state = 'F'
                    }
                'F' ->
                    if (tape.read() == 0) {
                        tape.write(1)
                        tape.left()
                        state = 'E'
                    } else {
                        tape.write(1)
                        tape.right()
                        state = 'A'
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