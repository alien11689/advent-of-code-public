package pl.touk.dpr.aoc2018

object Day14 {
    @JvmStatic
    fun main(args: Array<String>) {
        println(part1())
        println(part2())
    }

    private fun part1(): Any {
        val input = 640441

        val scores = mutableListOf(3, 7)

        val steps = 9

        val elves = mutableListOf(0, 1)

        while (scores.size < input + steps + 1) {
            val newValue = scores[elves[0]] + scores[elves[1]]
            scores.addAll(newValue.toString().map { it.toString().toInt() })
            elves[0] = (elves[0] + scores[elves[0]] + 1) % scores.size
            elves[1] = (elves[1] + scores[elves[1]] + 1) % scores.size
        }
        return scores.subList(input, input + steps + 1).joinToString("")
    }

    private fun part2(): Any {
        val input = 640441
        val scores = mutableListOf(3, 7)

        val elves = mutableListOf(0, 1)

        val inputString = input.toString()

        while (true) {
            val newValue = scores[elves[0]] + scores[elves[1]]
            scores.addAll(newValue.toString().map { it.toString().toInt() })
            elves[0] = (elves[0] + scores[elves[0]] + 1) % scores.size
            elves[1] = (elves[1] + scores[elves[1]] + 1) % scores.size

            if (scores.size > inputString.length) {
                val substring = scores.subList(scores.size - inputString.length - 1, scores.size).joinToString("")
                if (substring.contains(inputString)) {
                    val toAdd = substring.indexOf(inputString)
                    return scores.size - inputString.length - (if (toAdd > 0) 0 else 1)
                }
            }
        }


    }
}