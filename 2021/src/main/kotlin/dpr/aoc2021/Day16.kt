package dpr.aoc2021

import dpr.commons.Util

object Day16 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/16/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private val mapping = mapOf(
        Pair('0', "0000"),
        Pair('1', "0001"),
        Pair('2', "0010"),
        Pair('3', "0011"),
        Pair('4', "0100"),
        Pair('5', "0101"),
        Pair('6', "0110"),
        Pair('7', "0111"),
        Pair('8', "1000"),
        Pair('9', "1001"),
        Pair('A', "1010"),
        Pair('B', "1011"),
        Pair('C', "1100"),
        Pair('D', "1101"),
        Pair('E', "1110"),
        Pair('F', "1111")
    )

    private fun part1(lines: List<String>): Any {
        val message = lines.first().map { mapping[it]!! }.joinToString("")
//        println("Message: $message")
        val result = readPackage(message, 0).second
        return result.sumVersions()
    }

    private fun readPackage(message: String, initCUr: Int): Pair<Int, Package> {
        var cur = initCUr
        val version = Integer.parseInt(message.substring(cur, cur + 3), 2)
        cur += 3
        val type = Integer.parseInt(message.substring(cur, cur + 3), 2)
        cur += 3
//        println("version $version, type $type")
        if (type == 4) {
            var literal = 0L
            while (true) {
                val start = message[cur]
                cur++
                literal = literal * 16 + Integer.parseInt(message.substring(cur, cur + 4), 2)
                cur += 4
                if (start == '0') {
                    break
                }
            }
            return Pair(cur, Package(version, type, literal))
        } else {
            if (message[cur] == '0') {
                cur++
                val count = Integer.parseInt(message.substring(cur, cur + 15), 2)
                cur += 15
                val next = cur + count
                val subPackages = mutableListOf<Package>()
                while (cur < next) {
//                    println("Substring: ${message.substring(cur, message.length)} reading from $cur till $next")
                    if (message.substring(cur, next).all { it == '0' }) {
                        cur = next
                    }
                    val subPackage = readPackage(message, cur)
                    cur = subPackage.first
                    subPackages.add(subPackage.second)
                }
                return Pair(cur, Package(version, type, null, subPackages))
            } else {
                cur++
                val countOfSubpackages = Integer.parseInt(message.substring(cur, cur + 11), 2)
//                println("Subpackages count $countOfSubpackages")
                cur += 11
                val subPackages = mutableListOf<Package>()
                for (i in 1..countOfSubpackages) {
                    val subPackage = readPackage(message, cur)
                    cur = subPackage.first
                    subPackages.add(subPackage.second)
                }
                return Pair(cur, Package(version, type, null, subPackages))
            }
        }
    }

    data class Package(val version: Int, val type: Int, val literal: Long? = null, val subPackages: List<Package> = emptyList()) {
        fun sumVersions(): Int {
            return version + subPackages.sumOf { it.sumVersions() }
        }

        fun doMath(): Long {
            return when (type) {
                4 -> literal!!
                0 -> subPackages.sumOf { it.doMath() }
                1 -> if (subPackages.isEmpty()) 0 else subPackages.map { it.doMath() }.fold(1) { acc, l -> acc * l }
                2 -> subPackages.minOf { it.doMath() }
                3 -> subPackages.maxOf { it.doMath() }
                5 -> if (subPackages[0].doMath() > subPackages[1].doMath()) 1 else 0
                6 -> if (subPackages[0].doMath() < subPackages[1].doMath()) 1 else 0
                7 -> if (subPackages[0].doMath() == subPackages[1].doMath()) 1 else 0
                else -> throw RuntimeException()
            }
        }
    }

    private fun part2(lines: List<String>): Any {
        val message = lines.first().map { mapping[it]!! }.joinToString("")
        val result = readPackage(message, 0).second
        return result.doMath()
    }
}


