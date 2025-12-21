package dpr.aoc2016

import dpr.commons.Util

object Day19 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = 3017957
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: Int): Int {
        val begin = Node(1)
        var cur = begin
        (2..input).forEach {
            val next = Node(it, prev = cur)
            cur.next = next
            cur = next
        }
        cur.next = begin
        begin.prev = cur
        cur = begin
        while (cur.next != cur) {
            cur.next = cur.next!!.next
            cur = cur.next!!
        }
        return cur.x
    }

    @JvmStatic
    fun part2(input: Int): Int {
        val begin = Node(1)
        var cur = begin
        var mid = begin
        (2..input).forEach {
            val next = Node(it, prev = cur)
            cur.next = next
            cur = next
            if (it == (input + 1) / 2) {
                mid = next
            }
        }
        cur.next = begin
        begin.prev = cur
        var i = 0
        cur = begin
        while (cur.next != cur.prev) {
//            println("Elves: $i/${size},  Current elf ${cur.x}, Across elf ${mid.x}")
            mid.delete()
            mid = mid.next!!
            cur = cur.next!!
            ++i
            if ((input - i) % 2 == 0) {
                mid = mid.next!!
            }
        }
        return cur.x
    }

    data class Node(val x: Int, var next: Node? = null, var prev: Node? = null) {
        fun delete() {
            prev!!.next = next
            next!!.prev = prev
        }
    }
}
