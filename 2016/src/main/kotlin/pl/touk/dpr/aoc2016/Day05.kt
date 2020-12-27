package pl.touk.dpr.aoc2016

import java.math.BigInteger
import java.security.MessageDigest

object Day05 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = "ojvtpuvg"
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        return generateSequence(0) { it + 1 }
                .map {
                    md5.reset()
                    val toHash = input + it
                    Pair(it, String.format("%032x", BigInteger(1, md5.digest(toHash.toByteArray()))))
                }
                .onEach { mem[it.first] = it.second }
                .filter { it.second.startsWith("00000") }
                .map { it.second[5] }
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
                String.format("%032x", BigInteger(1, md5.digest(toHash.toByteArray())))
            }
            if (hash.startsWith("00000") && hash[5] in ('0'..'7') && mutableList[hash[5].toString().toInt()] == null) {
                mutableList[hash[5].toString().toInt()] = hash[6]
                if (mutableList.all { it != null }) {
                    return mutableList.joinToString("")
                }
            }
            ++i
        }
    }

    private val md5 = MessageDigest.getInstance("MD5")
    private val mem = mutableMapOf<Int, String>()

}