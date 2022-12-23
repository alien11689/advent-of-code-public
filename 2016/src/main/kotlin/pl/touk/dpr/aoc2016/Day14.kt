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
        while (res.size < 64) {
            val hash = getMd5(input, i, mem)
            val triple = hasTriple(hash)
            if (triple != null) {
                if (((i + 1)..(i + 1000)).any { hasFive(getMd5(input, it, mem), triple) }) {
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
        var i = 0
        while (i < hash.length - 4) {
            if (hash[i] == c && hash[i] == hash[i + 1] && hash[i] == hash[i + 2] && hash[i] == hash[i + 3] && hash[i] == hash[i + 4]) {
                return true
            }
            ++i
        }
        return false
    }

    private fun part2(input: String): Any {
        val res = mutableListOf<Int>()
        val mem = mutableMapOf<Int, String>()
        var i = 0
        while (res.size < 64) {
            val hash = getMd5For2017(input, i, mem)
            val triple = hasTriple(hash)
            if (triple != null) {
                if (((i + 1)..(i + 1000)).any { hasFive(getMd5For2017(input, it, mem), triple) }) {
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
        val v = String.format("%032x", BigInteger(1, md5.digest("$input$idx".toByteArray())))
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
            res = String.format("%032x", BigInteger(1, md5.digest(res.toByteArray())))
        }
        mem[idx] = res
        return res
    }

    private val md5 = MessageDigest.getInstance("MD5")
}
