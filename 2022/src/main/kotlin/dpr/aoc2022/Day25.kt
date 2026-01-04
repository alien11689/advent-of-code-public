package dpr.aoc2022

import dpr.commons.Util

object Day25 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/25/input.txt")
        println(part1(lines))
    }

    @JvmStatic
    fun part1(lines: List<String>): String {
        return convertToSnafu(readInput(lines))
    }

    private fun convertToSnafu(init: Long): String {
        var number = init
        var base = 1L
        while (number > base) {
            base *= 5
        }
        if (base / 2 > number) {
            base /= 5
        }
//        println("Base $base")
        val sb = StringBuilder()
        while (base > 0) {
            var curDigit = number / base
            number -= base * curDigit
//            println("Semi result $curDigit, $number and $base")
            if (number >= 0 && number > base / 2) {
                ++curDigit
                number -= base
            } else if (number < 0 && number < -base / 2) {
                --curDigit
                number += base
            }
//            println("Semi result2 $curDigit, $number and $base")
            when (curDigit) {
                -1L -> sb.append("-")
                -2L -> sb.append("=")
                in 0..2 -> sb.append(curDigit)
                else -> throw RuntimeException("Unknown digit $curDigit")
            }
            base /= 5
//            println("Cur $number and $base")
        }

        return sb.toString()
    }

    private fun readInput(lines: List<String>): Long {
        val sum = lines.sumOf { line ->
            var base = 1L
            var value = 0L
            //            println("reading line $line as ${line.reversed().toCharArray().map { it }}")
            line.reversed().toCharArray().forEach {
                val number = when (it) {
                    '1' -> 1
                    '2' -> 2
                    '0' -> 0
                    '-' -> -1
                    '=' -> -2
                    else -> throw RuntimeException()
                }
                //                println("Current $value plus $base * $number")
                value += base * number
                base *= 5
            }
            //            println("$line => $value ")
            value
        }
        return sum
    }
}

