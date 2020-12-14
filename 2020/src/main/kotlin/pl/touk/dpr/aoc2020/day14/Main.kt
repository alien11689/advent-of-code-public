package pl.touk.dpr.aoc2020.day14

import pl.touk.dpr.aoc2020.Util

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/14/input.txt")
//        println(part1("""mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
//mem[8] = 11
//mem[7] = 101
//mem[8] = 0""".lines()))
        println(part1(input))
//        println(part2("""mask = 000000000000000000000000000000X1001X
//mem[42] = 100
//mask = 00000000000000000000000000000000X0XX
//mem[26] = 1""".lines()))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val memory = mutableMapOf<Long, Long>()
        var mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        input.map { it.split(Regex("[ =\\[\\]]+")).toList() }.forEach {
            if (it.size == 2) {
                // set mask
                mask = it[1]
            } else {
                val address = it[1].toLong()
                val value = it[2].toLong().toString(2).padStart(36, '0').foldIndexed(0L) { index, acc, c ->
                    val v = if (mask[index] == 'X') {
                        c
                    } else {
                        mask[index]
                    }
                    val vv = if (v == '1') 1 else 0
                    acc * 2 + vv
                }
                memory[address] = value
            }
        }
        return memory.values.sum().toString()
    }

    private fun part2(input: List<String>): Any {
        val memory = mutableMapOf<Long, Long>()
        var mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        input.map { it.split(Regex("[ =\\[\\]]+")).toList() }.forEach {
            if (it.size == 2) {
                // set mask
                mask = it[1]
            } else {
                val value = it[2].toLong()
                val address = it[1].toLong().toString(2).padStart(36, '0').mapIndexed { index, c ->
                    val v = if (mask[index] == '0') {
                        c
                    } else if (mask[index] == '1')
                        '1'
                    else {
                        mask[index]
                    }
                    v
                }
                var s = setOf(0L)
                address.forEach { c ->
                    s = s.flatMap { cur ->
                        if (c == 'X') {
                            listOf(cur * 2, cur * 2 + 1)
                        } else {
                            listOf(cur * 2 + if (c == '1') 1 else 0)
                        }
                    }.toSet()
                }
                s.forEach { address ->
                    memory[address] = value
                }
            }
        }
        return memory.values.sum().toString()
        // 1139874673521 is too low
    }
}

