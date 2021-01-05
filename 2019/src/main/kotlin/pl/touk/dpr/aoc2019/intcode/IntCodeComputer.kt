package pl.touk.dpr.aoc2019.intcode

import java.util.LinkedList
import java.util.Queue

object IntCodeComputer {

    fun parseInput(input: String): MutableMap<Long, Long> = parseInput(input.split(","))

    fun parseInput(input: List<String>): MutableMap<Long, Long> {
        val m = mutableMapOf<Long, Long>().withDefault { 0L }
        input.forEachIndexed { index, s ->
            m[index.toLong()] = s.toLong()
        }
        return m
    }

    fun instruction(input: Queue<Long>, text: String) {
        text.forEach {
            input.offer(it.toLong())
        }
        input.offer(10)
    }

    private fun param(v: MutableMap<Long, Long>, pos: Long, mode: Int, rel: Long): Long {
        if (mode == 0) {
            val l = v[pos]!!
            return v.getOrDefault(l, 0L)
        } else if (mode == 1) {
            return v[pos]!!
        } else {
            val l = v[pos]!!
            return v.getOrDefault(rel + l, 0L)
        }
    }

    private fun p1Mode(op: Int): Int {
        return (op / 100) % 10
    }

    private fun p2Mode(op: Int): Int {
        return (op / 1000) % 10
    }

    private fun p3Mode(op: Int): Int {
        return (op / 10000) % 10
    }

    fun assignTo(v: MutableMap<Long, Long>, pos: Long, mode: Int, value: Long, rel: Long) {
        when (mode) {
            0 -> v[v[pos]!!] = value
            1 -> v[pos] = value
            2 -> v[rel + v[pos]!!] = value
            else -> {
            }
        }
    }

    fun program(s: IntCodeComputerState, output: Queue<Long> = LinkedList()) {
        val v = s.v
        var pos = s.pos
        var rel = s.rel

        while (true) {
            val op = v[pos]!!.toInt()
            when (op % 100) {
                99 -> {
                    s.ended = true
                    return
                }
                1 -> {
                    assignTo(v, pos + 3, p3Mode(op), param(v, pos + 1, p1Mode(op), rel) + param(v, pos + 2, p2Mode(op), rel), rel)
                    pos += 4
                }
                2 -> {
                    assignTo(v, pos + 3, p3Mode(op), param(v, pos + 1, p1Mode(op), rel) * param(v, pos + 2, p2Mode(op), rel), rel)
                    pos += 4
                }
                5 -> {
                    if (param(v, pos + 1, p1Mode(op), rel) != 0L) {
                        pos = param(v, pos + 2, p2Mode(op), rel)
                    } else {
                        pos += 3
                    }
                }
                6 -> {
                    if (param(v, pos + 1, p1Mode(op), rel) == 0L) {
                        pos = param(v, pos + 2, p2Mode(op), rel)
                    } else {
                        pos += 3
                    }
                }
                7 -> {
                    if (param(v, pos + 1, p1Mode(op), rel) < param(v, pos + 2, p2Mode(op), rel)) {
                        assignTo(v, pos + 3, p3Mode(op), 1, rel)
                    } else {
                        assignTo(v, pos + 3, p3Mode(op), 0, rel)
                    }
                    pos += 4
                }
                8 -> {
                    if (param(v, pos + 1, p1Mode(op), rel) == param(v, pos + 2, p2Mode(op), rel)) {
                        assignTo(v, pos + 3, p3Mode(op), 1, rel)
                    } else {
                        assignTo(v, pos + 3, p3Mode(op), 0, rel)
                    }
                    pos += 4
                }
                9 -> {
                    val shift = param(v, pos + 1, p1Mode(op), rel)
                    rel += shift
                    pos += 2
                }
                3 -> {
                    if (s.input.isEmpty()) {
                        s.v = v
                        s.pos = pos
                        s.rel = rel
                        //println("Empty input")
                        return
                    }
                    //println "Get input from $s.input"
                    assignTo(v, pos + 1, p1Mode(op), s.input.poll(), rel)
                    pos += 2
                }
                4 -> {
                    val out = param(v, pos + 1, p1Mode(op), rel)
//                println "Out: $out"
                    output.offer(out)
                    pos += 2
                }
                else -> {
                    println("ERROR $op")
                    throw  NullPointerException()
                }
            }
        }
    }

}


data class IntCodeComputerState(
        var v: MutableMap<Long, Long>,
        var pos: Long = 0,
        val input: LinkedList<Long> = LinkedList(),
        var ended: Boolean = false,
        var rel: Long = 0
) 
