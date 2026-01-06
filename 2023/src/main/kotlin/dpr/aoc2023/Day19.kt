package dpr.aoc2023

import dpr.commons.Util
import java.util.Stack
import kotlin.math.max
import kotlin.math.min

object Day19 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/19/input.txt")
        val (ruleLists, items) = parseInput(lines)
        println(part1(ruleLists, items))
        println(part2(ruleLists))
    }

    enum class Sign {
        LT,
        GT,
        ALWAYS;
    }

    data class Rule(val condition: Condition, val target: String) {
        fun accepts(item: Map<String, Int>): Boolean = condition.test(item)
    }

    @JvmStatic
    fun part1(ruleLists: Map<String, List<Rule>>, items: List<Map<String, Int>>): Int {
        return items.sumOf { if (pipeline(it, ruleLists)) it.values.sum() else 0 }
    }

    @JvmStatic
    fun parseInput(lines: List<String>): Pair<Map<String, List<Rule>>, List<Map<String, Int>>> {
        val ruleLists = mutableMapOf<String, List<Rule>>()
        val items = mutableListOf<Map<String, Int>>()
        lines.forEach { line ->
            if (line.startsWith("{")) {
                items.add(parseItem(line))
            } else {
                val (name, rulesList) = parseRuleList(line)
                ruleLists[name] = rulesList
            }
        }
        return Pair(simplifyBasicRules(ruleLists), items)
    }

    private fun pipeline(item: Map<String, Int>, ruleLists: Map<String, List<Rule>>): Boolean {
        var cur = "in"
        while (true) {
            when (val result = applyOn(item, ruleLists[cur]!!)) {
                "A" -> return true
                "R" -> return false
                else -> {
                    cur = result
                }
            }
        }
    }

    private fun applyOn(item: Map<String, Int>, rules: List<Rule>): String {
        for (r in rules) {
            if (r.accepts(item)) {
                return r.target
            }
        }
        throw RuntimeException("Item $item and rules $rules")
    }

    private fun parseItem(line: String): Map<String, Int> {
        return line.split(Regex("[{}=,]+"))
            .drop(1)
            .chunked(2)
            .filter { it.size == 2 }
            .associate { it[0] to it[1].toInt() }
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
                Rule(Condition(name, sign, value.toInt()), target)
            } else {
                Rule(Condition.ALWAYS, ruleString)
            }
        }
    }

    data class Condition(val part: String, val sign: Sign, val value: Int, val negated: Boolean = false) {
        fun negate(): Condition = this.copy(negated = !negated)

        fun test(item: Map<String, Int>): Boolean {
            if (part !in item) {
                return true
            }
            return when (sign) {
                Sign.ALWAYS -> true
                Sign.LT -> item[part]!! < value
                else -> item[part]!! > value
            }
        }

        fun applyOn(acc: Map<String, ItemRange>): Map<String, ItemRange> {
            return acc.map { (key, value) ->
                if (part == key) {
                    key to limit(acc[key]!!)
                } else {
                    key to value
                }
            }.toMap()
        }

        private fun limit(itemRange: ItemRange): ItemRange {
            return when {
                sign == Sign.LT && !negated -> itemRange.copy(to = min(itemRange.to, value - 1))
                sign == Sign.LT && negated -> itemRange.copy(from = max(itemRange.from, value))
                sign == Sign.GT && !negated -> itemRange.copy(from = max(itemRange.from, value + 1))
                sign == Sign.GT && negated -> itemRange.copy(to = min(itemRange.to, value))
                else -> throw RuntimeException("$this")
            }
        }

        companion object {
            val ALWAYS = Condition("", Sign.ALWAYS, 0)
        }
    }

    data class Current(val ruleName: String, val conditions: Set<Condition>)

    data class ItemRange(val from: Int = 1, val to: Int = 4000) {
        fun count(): Int = if (from <= to) to - from + 1 else 0
    }

    @JvmStatic
    fun part2(ruleLists: Map<String, List<Rule>>): Long {
        val acceptingConditions = findPipelinesResolvingToAccepted(ruleLists)
        return acceptingConditions.sumOf { conditions ->
            val ranges = conditions.fold(listOf("x", "m", "a", "s").associateWith { ItemRange() }) { acc, cur ->
                cur.applyOn(acc)
            }
            ranges.values.fold(1L) { acc, cur -> acc * cur.count() }
        }
    }

    private fun findPipelinesResolvingToAccepted(ruleLists: Map<String, List<Rule>>): MutableList<Set<Condition>> {
        val stack = Stack<Current>()
        stack.push(Current("in", emptySet()))
        val acceptingConditions = mutableListOf<Set<Condition>>()
        while (stack.isNotEmpty()) {
            val cur = stack.pop()
            val rules = ruleLists[cur.ruleName]!!
            var prevConditionsNegated = emptySet<Condition>()
            rules.forEach {
                val condition = it.condition
                when (it.target) {
                    "A" -> acceptingConditions.add(
                        cur.conditions + prevConditionsNegated + if (condition.sign != Sign.ALWAYS) setOf(
                            condition
                        ) else emptySet()
                    )

                    "R" -> {}
                    else -> stack.push(
                        Current(
                            it.target,
                            cur.conditions + prevConditionsNegated + if (condition.sign != Sign.ALWAYS) setOf(condition) else emptySet()
                        )
                    )
                }
                prevConditionsNegated = prevConditionsNegated + condition.negate()
            }
        }
        return acceptingConditions
    }

    private fun simplifyBasicRules(ruleLists: Map<String, List<Rule>>): Map<String, List<Rule>> {
        var current = ruleLists
        while (true) {
            //            println("Simplified")
            var simplified = current.map { simplify(it) }.toMap()
            if (simplified == current) {
                return current
            }
            val reducedRules = simplified.filter { it.value.size == 1 }
            simplified = simplified - reducedRules.keys
            simplified = replaceTarget(simplified, reducedRules)
            //            println(reducedRules)
            current = simplified
        }
    }

    private fun replaceTarget(
        simplified: Map<String, List<Rule>>,
        reducedRules: Map<String, List<Rule>>
    ): Map<String, List<Rule>> {
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
            ruleList.key to rules.take(size - 2) + Rule(Condition.ALWAYS, rules[size - 1].target)
        } else {
            ruleList.key to rules
        }
    }
}

