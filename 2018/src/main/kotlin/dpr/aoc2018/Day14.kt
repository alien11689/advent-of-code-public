package dpr.aoc2018

import dpr.commons.Util

object Day14 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        println(part1())
        println(part2())
    }

    @JvmStatic
    fun part1(input: Int = 640441): String {
        val scores = mutableListOf(3, 7)

        val steps = 9

        val elves = mutableListOf(0, 1)

        while (scores.size < input + steps + 1) {
            play(scores, elves)
        }
        return scores.subList(input, input + steps + 1).joinToString("")
    }

    @JvmStatic
    fun part2(input: String = "640441"): Int {
        val scores = mutableListOf(3, 7)

        val elves = mutableListOf(0, 1)

        while (true) {
            play(scores, elves)

            if (scores.size > input.length) {
                val substring = scores.subList(scores.size - input.length - 1, scores.size).joinToString("")
                if (substring.contains(input)) {
                    val toAdd = substring.indexOf(input)
                    return scores.size - input.length - (if (toAdd > 0) 0 else 1)
                }
            }
        }


    }

    private fun play(scores: MutableList<Int>, elves: MutableList<Int>) {
        val newValue = scores[elves[0]] + scores[elves[1]]
        scores.addAll(newValue.toString().map { it.toString().toInt() })
        elves[0] = (elves[0] + scores[elves[0]] + 1) % scores.size
        elves[1] = (elves[1] + scores[elves[1]] + 1) % scores.size
    }
}
