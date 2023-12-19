package dpr.aoc2023

import dpr.commons.Util
import java.util.UUID

object Day19 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/19/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/19/test1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    enum class Sign {
        LT,
        GT,
        ALWAYS
    }

    data class Rule(val part: String, val sign: Sign, val value: Long, val target: String) {
        fun accepts(item: Map<String, Long>): Boolean {
            return when (sign) {
                Sign.ALWAYS -> true
                Sign.LT -> item[part]!! < value
                else -> item[part]!! > value
            }
        }

    }

    private fun part1(lines: List<String>): Any {
        val ruleLists = mutableMapOf<String, List<Rule>>()
        val items = mutableListOf<Map<String, Long>>()
        lines.forEach { line ->
            if (line.startsWith("{")) {
                items.add(parseItem(line))
            } else {
                val (name, rulesList) = parseRuleList(line)
                ruleLists[name] = rulesList
            }
        }
        return items.sumOf { if (pipeline(it, ruleLists)) it.values.sum() else 0L }
    }

    private fun pipeline(item: Map<String, Long>, ruleLists: MutableMap<String, List<Rule>>): Boolean {
        var cur = "in"
        while (true) {
            val result = applyOn(item, ruleLists[cur]!!)
            when (result) {
                "A" -> return true
                "R" -> return false
                else -> {
                    cur = result
                }
            }
        }
    }

    private fun applyOn(item: Map<String, Long>, rules: List<Rule>): String {
        for (r in rules) {
            if(r.accepts(item)){
                return r.target
            }
        }
        throw RuntimeException("Item $item and rules $rules")
    }

    private fun parseItem(line: String): Map<String, Long> {
        return line.split(Regex("[{}=,]+"))
            .drop(1)
            .chunked(2)
            .filter { it.size == 2 }
            .associate { it[0] to it[1].toLong() }
    }

    private fun parseRuleList(line: String): Pair<String, List<Rule>> {
        val (name, rulesString) = line.split(Regex("[{}]"))
        return name to parseRuleListString(rulesString)
    }

    private fun parseRuleListString(rulesString: String): List<Rule> {
        return rulesString.split(",").map { ruleString ->
            if (ruleString.contains(":")) {
                val sign = if (ruleString.contains("<")) Sign.LT else Sign.GT
                val (name, value, target) = ruleString.split(Regex("[<>:]"))
                Rule(name, sign, value.toLong(), target)
            } else {
                Rule(UUID.randomUUID().toString(), Sign.ALWAYS, 0, ruleString)
            }
        }
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

