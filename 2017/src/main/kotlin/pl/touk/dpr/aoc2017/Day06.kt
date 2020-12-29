package pl.touk.dpr.aoc2017

object Day06 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getFileContent("/06/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val blocks = input.split("\t").map { it.toInt() }.toMutableList()
        val memory = mutableSetOf(blocks.toList())
        var re = 0
        while (true) {
            re++
            var (max, maxI) = findMax(blocks)
            blocks[maxI] = 0
            while (max > 0) {
                --max
                maxI = (maxI + 1) % blocks.size
                blocks[maxI]++
            }
            if (blocks in memory) {
                return re
            }
            memory.add(blocks.toList())
        }
    }


    private fun part2(input: String): Any {
        val blocks = input.split("\t").map { it.toInt() }.toMutableList()
        val memory = mutableMapOf(Pair(blocks.toList(), 0))
        var re = 0
        while (true) {
            re++
            var (max, maxI) = findMax(blocks)
            blocks[maxI] = 0
            while (max > 0) {
                --max
                maxI = (maxI + 1) % blocks.size
                blocks[maxI]++
            }
            if (blocks in memory) {
                return re - memory[blocks.toList()]!!
            }
            memory[blocks.toList()] = re
        }
    }

    private fun findMax(b: List<Int>): Pair<Int, Int> {
        var max = b[0]
        var maxI = 0
        b.forEachIndexed { index, i ->
            if (i > max) {
                max = i
                maxI = index
            }
        }
        return Pair(max, maxI)
    }
}