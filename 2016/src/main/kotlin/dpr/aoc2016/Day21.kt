package dpr.aoc2016

import dpr.commons.Util

object Day21 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/21/input.txt")
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: List<String>, init: String = "abcdefgh"): String {
        val rules = parse(input)
        val password = init.toList()
        return scramble(rules, password)
    }

    private fun scramble(rules: List<Rule>, password: List<Char>): String {
        return rules.fold(password) { acc, rule ->
            rule.use(acc)
        }.joinToString("")
    }

    private fun parse(input: List<String>): List<Rule> {
        return input.map { inp ->
            val s = inp.split(" ")
            if (inp.startsWith("swap position")) {
                Rule.SwapPosition(s[2].toInt(), s[5].toInt())
            } else if (inp.startsWith("swap letter")) {
                Rule.SwapLetter(s[2][0], s[5][0])
            } else if (inp.startsWith("rotate left")) {
                Rule.Rotate(-(s[2].toInt()))
            } else if (inp.startsWith("rotate right")) {
                Rule.Rotate(s[2].toInt())
            } else if (inp.startsWith("rotate based")) {
                Rule.RotateBased(s[6][0])
            } else if (inp.startsWith("reverse")) {
                Rule.Reverse(s[2].toInt(), s[4].toInt())
            } else if (inp.startsWith("move")) {
                Rule.Move(s[2].toInt(), s[5].toInt())
            } else {
                throw RuntimeException("No rule $inp")
            }
        }
    }

    @JvmStatic
    fun part2(input: List<String>, target: String = "fbgdceah"): String {
        val rules = parse(input)
        val password = target.toList()
        return permutations(password.toSet()).find { scramble(rules, it) == target }!!.joinToString("")
    }

    private fun permutations(input: Set<Char>): List<List<Char>> {
        return if (input.size == 1) {
            listOf(input.toList())
        } else {
            input.flatMap { cur ->
                permutations(input - cur).map { it + cur }
            }
        }
    }

    sealed class Rule {
        abstract fun use(input: List<Char>): List<Char>

        data class SwapPosition(val from: Int, val to: Int) : Rule() {
            override fun use(input: List<Char>): List<Char> {
                return input.mapIndexed { index, c ->
                    when (index) {
                        from -> input[to]
                        to -> input[from]
                        else -> c
                    }
                }
            }
        }

        data class SwapLetter(val from: Char, val to: Char) : Rule() {
            override fun use(input: List<Char>): List<Char> {
                return input.map { if (it == from) to else if (it == to) from else it }
            }
        }

        data class Rotate(val amount: Int) : Rule() {
            override fun use(input: List<Char>): List<Char> {
                val out = input.toMutableList()
                for (i in input.indices) {
                    out[(input.size + i + amount) % input.size] = input[i]
                }
                return out
            }
        }

        data class RotateBased(val letter: Char) : Rule() {
            override fun use(input: List<Char>): List<Char> {
                val letterIdx = input.indexOf(letter)
                return Rotate(letterIdx + if (letterIdx >= 4) 2 else 1).use(input)
            }
        }

        data class Reverse(val from: Int, val to: Int) : Rule() {
            override fun use(input: List<Char>): List<Char> {
                val reversed = input.slice(from..to).reversed()
                return input.take(from) + reversed + input.drop(to + 1)
            }
        }

        data class Move(val from: Int, val to: Int) : Rule() {
            override fun use(input: List<Char>): List<Char> {
                val out = input.toMutableList()
                val a = input[from]
                out.removeAt(from)
                if (out.size > to) {
                    out.add(to, a)
                } else {
                    out.add(a)
                }
                return out
            }
        }
    }
}
