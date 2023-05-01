package dpr.aoc2017

object Day10 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
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
        return knotHash(text)

    }

    fun knotHash(text: String): String {
        val lengths = text.map { it.code } + listOf(17, 31, 73, 47, 23)

        var curPos = 0
        var skipSize = 0

        val inp = (0..255).toMutableList()

        repeat(64) {
            val p = processLengths(lengths, inp, curPos, skipSize)
            curPos = p.first
            skipSize = p.second
        }

        return inp.chunked(16)
                .map { it.reduce { a, b -> a.xor(b) } }
                .joinToString("") { String.format("%02x", it) }
    }

    private fun processLengths(lengths: List<Int>, inp: MutableList<Int>, curPos: Int, skipSize: Int): Pair<Int, Int> {
        var curPos1 = curPos
        var skipSize1 = skipSize
        lengths.forEach { length ->
            val reversed = (0 until length).map {
                inp[(curPos1 + it) % inp.size]
            }.reversed()
            repeat(length) {
                inp[(curPos1 + it) % inp.size] = reversed[it]
            }
            curPos1 = (curPos1 + length + skipSize1) % inp.size
            ++skipSize1
        }
        return curPos1 to skipSize1
    }
}
