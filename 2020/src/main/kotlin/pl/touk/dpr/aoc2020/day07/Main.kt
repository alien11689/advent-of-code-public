package pl.touk.dpr.aoc2020.day07

import pl.touk.dpr.aoc2020.Util
import java.util.Stack

object Main {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getFileContent("/07/input.txt")
        part1(input)
        part2(input)
    }

    private fun part1(input: String) {
        val rules = input.lines()
                .filter { it.isNotEmpty() }
                .map { Rule.parse(it) }
        val target = Bag("shiny", "gold")
        val stack = Stack<Bag>()
        stack.push(target)
        val needed = mutableSetOf<Bag>()
        while (!stack.empty()) {
            val current = stack.pop()
            if (current in needed) {
                continue
            }
            needed.add(current)
            val foundIn = rules.filter { it.to.keys.contains(current) }.map { it.from }
            foundIn.forEach { stack.push(it) }
        }
        println(needed.size - 1)
    }

    private fun part2(input: String) {
        val rules = input.lines()
                .filter { it.isNotEmpty() }
                .map { Rule.parse(it) }
        val target = Bag("shiny", "gold")
        var count = 0
        val stack = Stack<Bag>()
        stack.push(target)
        while (!stack.empty()) {
            val current = stack.pop()
            val rule = rules.find { it.from == current }!!
            rule.to.forEach { entry ->
                (1..entry.value).forEach {
                    stack.push(entry.key)
                }
                count += entry.value
            }
        }
        // 121 is too low
        println(count)
    }

    data class Bag(val type: String, val color: String)

    data class Rule(val from: Bag, val to: Map<Bag, Int>) {
        companion object {
            fun parse(line: String): Rule {
                val parts = line.split(Regex("[ ,]+")).toList()
                val from = Bag(parts[0], parts[1])
                val contains = mutableMapOf<Bag, Int>()
                if (!line.endsWith("no other bags.")) {
                    var i = 4
                    while (parts.size >= i + 3) {
                        contains[Bag(parts[i + 1], parts[i + 2])] = parts[i].toInt()
                        i += 4
                    }
                }
                return Rule(from, contains)
            }
        }
    }
}