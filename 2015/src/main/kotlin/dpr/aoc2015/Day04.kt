package dpr.aoc2015


import dpr.commons.Util
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
        val zero = 0.toByte()
        val f = 15.toByte()
        val expectedInitialZero = prefix.length
        return generateSequence(init) { it + 1 }
            .first {
                val toHash = input + it
                val hash = md5.digest(toHash.toByteArray())
                hash[0] == zero && hash[1] == zero && (
                    if (expectedInitialZero == 5) hash[2] in zero..f else hash[2] == zero)
//                    val res = String.format("%032x", BigInteger(1, hash))
//                    res.startsWith(prefix)
            }
    }
}
