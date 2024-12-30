package dpr.aoc2019

import dpr.commons.Util

object Day08 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getFileContent("/08/input.txt").trim()
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: String): Any {
        val row = 25
        val tall = 6

        val sol = input.chunked(row * tall).minByOrNull { it.filter { letter -> letter == '0' }.length }!!
        return sol.filter { it == '1' }.length * sol.filter { it == '2' }.length
    }

    private fun part2(input: String): String {
        val row = 25
        val tall = 6
        val black = '0'
//        val white = '1'
        val transparent = '2'

        val layers = input.chunked(row * tall)

        val sb = StringBuilder()
        for (high in 0 until tall) {
            for (r in 0 until row) {
                val cur = layers.map { it[high * row + r] }.find { it != transparent }
                sb.append(if (cur == black) ' ' else 'X')
            }
            if (high < tall - 1) {
                sb.append("\n")
            }
        }
        return sb.toString()
    }
}
