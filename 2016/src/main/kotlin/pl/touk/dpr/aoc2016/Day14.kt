package pl.touk.dpr.aoc2016

import java.math.BigInteger
import java.security.MessageDigest

object Day14 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = "yjdafjpo"
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val res = mutableListOf<Int>()
        val mem = mutableMapOf<Int, String>()
        var i = 0
        val range = (1..1000).toList()
        while (res.size < 64) {
            val hash = getMd5(input, i, mem)
            hasTriple(hash)?.let { triple ->
                if (range.any { hasFive(getMd5(input, it + i, mem), triple) }) {
                    res.add(i)
                }
            }
            ++i
        }
        return res.last()
    }

    private fun hasTriple(hash: String): Char? {
        var i = 0
        while (i < hash.length - 2) {
            if (hash[i] == hash[i + 1] && hash[i] == hash[i + 2]) {
                return hash[i]
            }
            ++i
        }
        return null
    }

    private fun hasFive(hash: String, c: Char): Boolean {
        if (hasTriple(hash) == c) {
            var i = 0
            while (i < hash.length - 4) {
                if (hash[i] == c && hash[i] == hash[i + 1] && hash[i] == hash[i + 2] && hash[i] == hash[i + 3] && hash[i] == hash[i + 4]) {
                    return true
                }
                ++i
            }
        }
        return false
    }

    private fun part2(input: String): Any {
        val res = mutableListOf<Int>()
        val mem = mutableMapOf<Int, String>()
        var i = 0
        val range = (1..1000).toList()
        while (res.size < 64) {
            val hash = getMd5For2017(input, i, mem)
            hasTriple(hash)?.let { triple ->
                if (range.any { hasFive(getMd5For2017(input, it + i, mem), triple) }) {
                    res.add(i)
                }
            }
            ++i
        }
        return res.last()
    }

    private fun getMd5(input: String, idx: Int, mem: MutableMap<Int, String>): String {
        if (idx in mem) {
            return mem[idx]!!
        }
        md5.reset()
        val v = hashToString(md5.digest("$input$idx".toByteArray()))
        mem[idx] = v
        return v
    }

    private fun getMd5For2017(input: String, idx: Int, mem: MutableMap<Int, String>): String {
        if (idx in mem) {
            return mem[idx]!!
        }
        var res = "$input$idx"
        repeat(2017) {
            md5.reset()
            res = hashToString(md5.digest(res.toByteArray()))
        }
        mem[idx] = res
        return res
    }

    private fun hashToString(hash: ByteArray) =
            BigInteger(1, hash).toString(16).let {
                when (it.length) {
                    32 -> it
                    31 -> "0$it"
                    30 -> "00$it"
                    29 -> "000$it"
                    28 -> "0000$it"
                    27 -> "00000$it"
                    26 -> "000000$it"
                    else -> throw RuntimeException("${it.length}")
                }
            }
//            String.format("%032x", BigInteger(1, hash))

    private val md5 = MessageDigest.getInstance("MD5")
}
