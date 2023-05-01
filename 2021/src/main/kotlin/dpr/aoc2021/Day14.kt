package dpr.aoc2021

import java.math.BigInteger
import java.util.LinkedList

object Day14 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/14/input.txt")
        println(part1(lines))
        println(part2b(lines))
    }

    private fun part1(lines: List<String>): Any {
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

//    private fun part2(lines: List<String>): Any {
//        var template = lines.first().toList()
//        val mm = HashMap<Pair<Char, Char>, Char>()
//        val instructions = lines.subList(1, lines.size)
//            .map { it.split(" -> ") }
//            .map { Instr(Pair(it[0][0], it[0][1]), it[1][0]) }
//            .forEach {
//                mm[it.pattern] = it.insert
//            }
//        mm.toList().sortedBy { it.first.first }.forEach(::println)
//        // ((B, H), B)
//        // ((F, C), F)
//        // ((H, B), H)
//        val root = Node(template.first(), null)
//        template.subList(1, template.size).fold(root) { acc, c ->
//            val node = Node(c, null, acc)
//            acc.next = node
//            node
//        }
////        println(first)
//        val countsInit = mutableMapOf<Char, Long>()
//        for (i in 1..40) {
//            println("Step $i")
//            var cur = root
//            var iter = 0
//            while (cur.next != null) {
////                println(++iter)
//                val next = cur.next!!
//                val k = Pair(cur.c, cur.next!!.c)
//                if (k in mm) {
//                    val insert = mm[k]!!
////                    if (cur.c == insert && cur.prev != null && cur.prev!!.c != insert) {
////                        countsInit[insert] = (countsInit[insert] ?: 0L) + 1
////                    } else {
//                        val node = Node(insert, cur.next, cur)
//                        cur.next!!.prev = node
//                        cur.next = node
////                    }
//                }else{
//                    println("Unused $k")
//                }
//                cur = next
//            }
////            var tmp: Node? = root
////            val res = mutableMapOf<Char, Long>()
////            while (tmp != null) {
////                res[tmp.c] = (res[tmp.c] ?: 0L) + 1
////                tmp = tmp.next
////            }
////            println(res)
////            println(res.values.maxOrNull()!! - res.values.minOrNull()!!)
//        }
////        println(countsInit)
//        var cur: Node? = root
//        while (cur != null) {
//            countsInit[cur.c] = (countsInit[cur.c] ?: 0L) + 1
//            cur = cur.next
//        }
//        return countsInit.values.maxOrNull()!! - countsInit.values.minOrNull()!!
//    }
//
//    data class Node(val c: Char, var next: Node?, var prev: Node? = null)

    private fun part2b(lines: List<String>): Any {
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

