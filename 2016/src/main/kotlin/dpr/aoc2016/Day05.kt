package dpr.aoc2016

import java.math.BigInteger
import java.security.MessageDigest

object Day05 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = "ojvtpuvg"
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        return generateSequence(0) { it + 1 }
                .map {
                    md5.reset()
                    val toHash = input + it
                    val hash = md5.digest(toHash.toByteArray())
                    mem[it] = hash
                    hash
                }
                .filter { hash -> hash[0] == zero && hash[1] == zero && hash[2] >= zero && hash[2] <= f }
                .map { String.format("%032x", BigInteger(1, it)) }
                .filter { it.startsWith("00000") } // to be sure
                .map { it[5] }
                .take(8)
                .joinToString("")
    }

    private fun part2(input: String): Any {
        var i = 0
        val mutableList: MutableList<Char?> = (0..7).map { null }.toMutableList()
        while (true) {
            val hash = if (i in mem) {
                mem[i]!!
            } else {
                md5.reset()
                val toHash = input + i
                md5.digest(toHash.toByteArray())
            }
            if (hash[0] == zero && hash[1] == zero && hash[2] >= zero && hash[2] <= f) {
                val hashString = String.format("%032x", BigInteger(1, hash))
                if (hashString.startsWith("00000") && hashString[5] in ('0'..'7') && mutableList[hashString[5].toString().toInt()] == null) {
                    mutableList[hashString[5].toString().toInt()] = hashString[6]
                    if (mutableList.all { it != null }) {
                        return mutableList.joinToString("")
                    }
                }
            }
            ++i
        }
    }

    private val md5 = MessageDigest.getInstance("MD5")
    private val mem = mutableMapOf<Int, ByteArray>()
    private const val zero = 0.toByte()
    private const val f = 15.toByte()

}
