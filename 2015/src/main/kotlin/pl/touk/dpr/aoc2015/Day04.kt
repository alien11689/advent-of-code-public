package pl.touk.dpr.aoc2015

import java.math.BigInteger
import java.security.MessageDigest


object Day04 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = "iwrupvqb"
        println(part1And2(input, "00000"))
        println(part1And2(input, "000000"))
    }

    private fun part1And2(input: String, prefix: String): Int {
        val md5 = MessageDigest.getInstance("MD5")
        return generateSequence(0) { it + 1 }
                .map {
                    md5.reset()
                    val toHash = input + it
                    Pair(it, String.format("%032x", BigInteger(1, md5.digest(toHash.toByteArray()))))
                }
                .filter { it.second.startsWith(prefix) }
                .take(1)
                .first().first
    }
}