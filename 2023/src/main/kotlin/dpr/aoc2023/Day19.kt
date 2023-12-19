package dpr.aoc2023

import dpr.commons.Util

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
            if (part !in item) {
                return true
            }
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
            if (r.accepts(item)) {
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
                Rule("", Sign.ALWAYS, 0, ruleString)
            }
        }
    }

    private fun part2(lines: List<String>): Any {
        var ruleLists = readRuleLists(lines)
        ruleLists = simplifyBasicRules(ruleLists)
        println(ruleLists)
        TODO()
    }

    private fun simplifyBasicRules(ruleLists: Map<String, List<Rule>>): Map<String, List<Rule>> {
        var ruleLists1 = ruleLists
        while (true) {
    //            println("Simplified")
            var simplified = ruleLists1.map { simplify(it) }.toMap()
            if (simplified == ruleLists1) {
                break
            }
            val reducedRules = simplified.filter { it.value.size == 1 }
            simplified = simplified - reducedRules.keys
            simplified = replaceTarget(simplified, reducedRules)
    //            println(reducedRules)
            ruleLists1 = simplified
        }
        return ruleLists1
    }

    private fun replaceTarget(simplified: Map<String, List<Rule>>, reducedRules: Map<String, List<Rule>>): Map<String, List<Rule>> {
        return simplified.map { (key, value) ->
            if (value.any { it.target in reducedRules.keys }) {
                key to shortcut(value, reducedRules)
            } else {
                key to value
            }
        }.toMap()
    }

    private fun shortcut(rules: List<Rule>, reducedRules: Map<String, List<Rule>>): List<Rule> {
        return rules.map { if (it.target in reducedRules) it.copy(target = reducedRules[it.target]!!.first().target) else it }
    }

    private fun simplify(ruleList: Map.Entry<String, List<Rule>>): Pair<String, List<Rule>> {
        val rules = ruleList.value
        val size = rules.size
        return if (rules[size - 1].target == rules[size - 2].target) {
            ruleList.key to rules.take(size - 2) + Rule("", Sign.ALWAYS, 0L, rules[size - 1].target)
        } else {
            ruleList.key to rules
        }
    }

    private fun readRuleLists(lines: List<String>): Map<String, List<Rule>> {
        val ruleLists = mutableMapOf<String, List<Rule>>()
        lines.forEach { line ->
            if (!line.startsWith("{")) {
                val (name, rulesList) = parseRuleList(line)
                ruleLists[name] = rulesList
            }
        }
        return ruleLists.toMap()
    }
}

