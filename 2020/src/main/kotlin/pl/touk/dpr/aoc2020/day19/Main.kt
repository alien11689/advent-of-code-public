package pl.touk.dpr.aoc2020.day19

import pl.touk.dpr.aoc2020.Util

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getLinesFromFile("/19/input.txt")
        println(part1(input))
        val input2 = Util.getLinesFromFile("/19/input2.txt")
        println(part2(input2))
    }

    private fun part1(input: List<String>): Any {
        val (rules, text) = parseInput(input)
        val rule0Raw = rules[0]!!
        val rule0 = resolveRule(rule0Raw, rules)
        val reg = Regex("^${rule0.toString()}$")
        return text.count { reg.matches(it) }
    }

    private fun parseInput(input: List<String>): Pair<MutableMap<Int, Rule>, MutableSet<String>> {
        var i = 0
        val rules = mutableMapOf<Int, Rule>()
        val text = mutableSetOf<String>()
        while (i < input.size) {
            val line = input[i]
            if (line.contains(":")) {
                // rule
                val parts = line.split(Regex("[: ]+"))
                val num = parts[0].toInt()
                val ruleParts = mutableListOf<Rule>()
                parts.forEachIndexed { index, s ->
                    if (index != 0) {
                        if (s == "|") {
                            ruleParts.add(Rule.Or)
                        } else if (s.startsWith("\"")) {
                            ruleParts.add(Rule.Text(s[1].toString()))
                        } else {
                            ruleParts.add(Rule.Num(s.toInt()))
                        }
                    }
                }
                rules.put(num, if (ruleParts.size == 1) ruleParts[0] else Rule.Complex(ruleParts))
            } else if (!line.isEmpty()) {
                text.add(line)
            }
            ++i
        }
        return Pair(rules, text)
    }

    private fun resolveRule(curRule: Rule, rules: MutableMap<Int, Rule>): Rule {
        return when (curRule) {
            Rule.Or -> Rule.Or
            is Rule.Num -> resolveRule(rules[curRule.n]!!, rules)
            is Rule.Text -> curRule
            is Rule.Complex -> Rule.Complex(curRule.parts.map { resolveRule(it, rules) })
        }
    }

    sealed class Rule {
        object Or : Rule() {
            override fun toString(): String {
                return "|"
            }
        }

        data class Num(val n: Int) : Rule()
        data class Text(val t: String) : Rule() {
            override fun toString(): String {
                return t
            }
        }

        data class Complex(val parts: List<Rule>) : Rule() {
            override fun toString(): String {
                return parts.joinToString(separator = "", prefix = "(", postfix = ")")
            }
        }
    }

    private fun part2(input: List<String>): Any {
        val (rules, text) = parseInput(input)
//        println("0: ${rules[0]}")
//        println("8: ${rules[8]}")
//        println("11: ${rules[11]}")
        val rule42 = resolveRule(rules[42]!!, rules)
        val rule31 = resolveRule(rules[31]!!, rules)
//        println("42: ${rule42}")
//        println("31: ${rule31}")
//        println("Rule 8 -> rule42+")
//        println("Rule 11 -> rule42 ... rule31")
        val initRegex = Regex("^($rule42)+($rule31)+$")
        val initSet = text.filter { initRegex.matches(it) }
        val exactRule42 = Regex("^$rule42$")
        val exactRule31 = Regex("^$rule31$")
        var res = 0
        initSet.forEach { s ->
//            println("Checking $s")
            var I = 0
            var i = I + 1
            var J = s.length
            var j = J - 1
            var ends = 0
            while (0 < j) {
                val toCheckEnd = s.substring(j, J)
                if (exactRule31.matches(toCheckEnd)) {
//                    println("Matched end $toCheckEnd")
                    J = j
                    ++ends
                }
                --j
            }
            var begins = 0
            while (i <= J) {
                val toCheckBegin = s.substring(I, i)
                if (exactRule42.matches(toCheckBegin)) {
//                    println("Matched begin $toCheckBegin")
                    I = i
                    ++begins
                }
                ++i
            }
//            println("$begins $ends")
            if (begins > ends) {
                ++res
            }
        }

        // 425 is too high
        return res
    }
}
