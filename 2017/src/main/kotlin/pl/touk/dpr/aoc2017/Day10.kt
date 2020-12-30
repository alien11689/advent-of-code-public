package pl.touk.dpr.aoc2017

object Day10 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getFileContent("/10/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val lengths = input.split(',').map { it.toInt() }

        var curPos = 0
        var skipSize = 0

        val inp = (0..255).toMutableList()

        lengths.forEach { length ->
            val reversed = (0 until length).map {
                inp[(curPos + it) % inp.size]
            }.reversed()
            (0 until length).forEach {
                inp[(curPos + it) % inp.size] = reversed[it]
            }
            curPos = (curPos + length + skipSize) % inp.size
            ++skipSize
        }

        return inp[0] * inp[1]
    }

    private fun part2(text: String): Any {
        val lengths = text.map { it.toInt() } + listOf(17, 31, 73, 47, 23)

        var curPos = 0
        var skipSize = 0

        val inp = (0..255).toMutableList()

        (1..64).forEach {
            lengths.forEach { length ->
                val reversed = (0 until length).map {
                    inp[(curPos + it) % inp.size]
                }.reversed()
                (0 until length).forEach {
                    inp[(curPos + it) % inp.size] = reversed[it]
                }
                curPos = (curPos + length + skipSize) % inp.size
                ++skipSize
            }

        }

        return inp.chunked(16)
                .map { it.reduce() { a, b -> a.xor(b) } }
                .joinToString("") { String.format("%02x", it) }

    }
}