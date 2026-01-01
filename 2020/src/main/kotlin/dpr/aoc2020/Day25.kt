package dpr.aoc2020

import dpr.commons.Util

object Day25 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/25/input.txt")
        println(part1(input))
    }

    @JvmStatic
    fun part1(input: List<String>): Long {
        val publicKey1 = input[0].toLong()
        val publicKey2 = input[1].toLong()
        val pn = 20201227
        val subject = 7
        var i = 1
        var res = 1L
        val ml = mutableMapOf<Long, Int>()
        while (ml.size != 1) {
            res = res * subject % pn
//            println("$i - $res $ml")
            if (res == publicKey1 || res == publicKey2) {
                ml[res] = i
            }
            ++i
        }
        i = 1
        res = 1L
        val keyToTransform = if (ml.containsKey(publicKey1)) publicKey2 else publicKey1
        val loop = ml.values.first()
        while (i <= loop) {
            res = res * keyToTransform % pn
//            println("$i - $res $ml")
            ++i
        }
        return res
    }
}
