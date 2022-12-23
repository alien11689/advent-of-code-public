package pl.touk.dpr.aoc2015

import java.math.BigInteger
import java.security.MessageDigest


object Day04 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = "iwrupvqb"
        val part1Result = part1And2(input, "00000")
        println(part1Result)
        println(part1And2(input, "000000", init = part1Result))
    }

    private fun part1And2(input: String, prefix: String, init: Int = 0): Int {
        val md5 = MessageDigest.getInstance("MD5")
        return generateSequence(init) { it + 1 }
                .first {
                    md5.reset()
                    val toHash = input + it
                    val res = String.format("%032x", BigInteger(1, md5.digest(toHash.toByteArray())))
                    res.startsWith(prefix)
                }
    }
}
