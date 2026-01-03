package dpr.aoc2021

import dpr.commons.Util
import java.math.BigInteger
import java.util.LinkedList

object Day14 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/14/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    @JvmStatic
    fun part1(lines: List<String>): Int {
        var template = lines.first().toList()
        val mm = parseReactions(lines)
        for (i in 1..10) {
            val newTemplate = LinkedList<Char>()
            newTemplate.add(template.first())
            template.zipWithNext().forEach {
                if (it in mm) {
                    newTemplate.add(mm[it]!!)
                }
                newTemplate.add(it.second)
            }
            template = newTemplate
        }
        val counts = template.fold(mutableMapOf<Char, Int>()) { acc, c ->
            acc[c] = (acc[c] ?: 0) + 1
            acc
        }.values
        return counts.maxOrNull()!! - counts.minOrNull()!!
    }

    private fun parseReactions(lines: List<String>): HashMap<Pair<Char, Char>, Char> {
        val mm = HashMap<Pair<Char, Char>, Char>()
        lines.subList(1, lines.size)
            .map { it.split(" -> ") }
            .map { Instr(Pair(it[0][0], it[0][1]), it[1][0]) }
            .forEach {
                mm[it.pattern] = it.insert
            }
        return mm
    }

    data class Instr(val pattern: Pair<Char, Char>, val insert: Char)

    @JvmStatic
    fun part2(lines: List<String>): BigInteger {
        val template = lines.first().toList()
        val mm = parseReactions(lines)
        var memory = mutableMapOf<Pair<Char, Char>, BigInteger>()
        template.zipWithNext().forEach { p ->
            memory[p] = (memory[p] ?: BigInteger.ZERO) + BigInteger.ONE
        }
        for (i in 1..40) {
            val newMem = mutableMapOf<Pair<Char, Char>, BigInteger>()
            memory.forEach { e ->
                val insert = mm[e.key]!!
                val left = Pair(e.key.first, insert)
                val right = Pair(insert, e.key.second)
                newMem[left] = (newMem[left] ?: BigInteger.ZERO) + memory[e.key]!!
                newMem[right] = (newMem[right] ?: BigInteger.ZERO) + memory[e.key]!!
            }
            memory = newMem
        }
        val counts = mutableMapOf<Char, BigInteger>()
        memory.forEach {
            counts[it.key.second] = (counts[it.key.second] ?: BigInteger.ZERO) + it.value
        }
        counts[template.first()] = counts[template.first()]!! + BigInteger.ONE
        return counts.values.maxOrNull()!! - counts.values.minOrNull()!!
    }
}

