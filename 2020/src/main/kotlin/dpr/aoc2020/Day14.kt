package dpr.aoc2020

import dpr.commons.Util

object Day14 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/14/input.txt")
        println(part1(input))
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
        input.map { it.split(Regex("[ =\\[\\]]+")).toList() }.forEach { parts ->
            if (parts.size == 2) {
                // set mask
                mask = parts[1]
            } else {
                val value = parts[2].toLong()
                val maskedAddress = parts[1].toLong().toString(2).padStart(36, '0').mapIndexed { index, c ->
                    val v = if (mask[index] == '0') {
                        c
                    } else if (mask[index] == '1')
                        '1'
                    else {
                        mask[index]
                    }
                    v
                }
                var addresses = setOf(0L)
                maskedAddress.forEach { c ->
                    addresses = addresses.flatMap { cur ->
                        if (c == 'X') {
                            listOf(cur * 2, cur * 2 + 1)
                        } else {
                            listOf(cur * 2 + if (c == '1') 1 else 0)
                        }
                    }.toSet()
                }
                addresses.forEach {
                    memory[it] = value
                }
            }
        }
        return memory.values.sum().toString()
        // 1139874673521 is too low
    }
}

