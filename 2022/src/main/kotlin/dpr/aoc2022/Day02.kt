package dpr.aoc2022

import dpr.commons.Util

object Day02 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/02/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    @JvmStatic fun part1(lines: List<String>): Int {
        return lines.sumOf {
            val (e, m) = it.split(" ")
            val enemy = interpretEnemy(e)
            val me = when (m) {
                "X" -> Option.ROCK
                "Y" -> Option.PAPER
                "Z" -> Option.SCISSORS
                else -> throw RuntimeException()
            }
            result(enemy, me)
        }
    }

    private fun result(enemy: Option, my: Option): Int {
        return when (Pair(enemy, my)) {
            Pair(Option.ROCK, Option.ROCK) -> 3
            Pair(Option.ROCK, Option.PAPER) -> 6
            Pair(Option.ROCK, Option.SCISSORS) -> 0
            Pair(Option.PAPER, Option.ROCK) -> 0
            Pair(Option.PAPER, Option.PAPER) -> 3
            Pair(Option.PAPER, Option.SCISSORS) -> 6
            Pair(Option.SCISSORS, Option.ROCK) -> 6
            Pair(Option.SCISSORS, Option.PAPER) -> 0
            Pair(Option.SCISSORS, Option.SCISSORS) -> 3
            else -> throw RuntimeException()
        } + my.score
    }

    enum class Option(val score: Int) {
        ROCK(1), PAPER(2), SCISSORS(3),
    }

    @JvmStatic fun part2(lines: List<String>): Int {
        return lines.sumOf {
            val (e, m) = it.split(" ")
            val enemy = interpretEnemy(e)
            val me = when (m) {
                "X" -> when (enemy) {
                    Option.ROCK -> Option.SCISSORS
                    Option.PAPER -> Option.ROCK
                    Option.SCISSORS -> Option.PAPER
                }

                "Y" -> enemy
                "Z" -> when (enemy) {
                    Option.ROCK -> Option.PAPER
                    Option.PAPER -> Option.SCISSORS
                    Option.SCISSORS -> Option.ROCK
                }

                else -> throw RuntimeException()
            }
            result(enemy, me)
        }
    }

    private fun interpretEnemy(e: String) = when (e) {
        "A" -> Option.ROCK
        "B" -> Option.PAPER
        "C" -> Option.SCISSORS
        else -> throw RuntimeException()
    }
}

