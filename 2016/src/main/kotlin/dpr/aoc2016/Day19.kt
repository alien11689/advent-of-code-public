package dpr.aoc2016

import dpr.commons.Util

object Day19 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = 3017957
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: Int): Any {
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

//    private fun part1Iter(input: Int): Any {
//        val elvesWithoutPresent = mutableSetOf<Int>()
//        var i = 0
//        while (elvesWithoutPresent.size < input - 1) {
//            val next = findNextWith(i, input, elvesWithoutPresent)
//            elvesWithoutPresent.add(next)
//            i = findNextWith(i, input, elvesWithoutPresent)
//        }
//        return i + 1
//    }

//    private fun findNextWith(cur: Int, all: Int, ewp: Set<Int>): Int {
//        var prev = cur
//        while (true) {
//            val next = (prev + 1) % all
//            if (next == cur || next !in ewp) {
//                return next
//            }
//            prev = next
//        }
//    }

    private fun part2(input: Int): Any {
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

//    private fun part2Iterative(input: Int): Any {
//        val elves = ArrayList<Int>()
//        elves.addAll((1..input))
//        var i = 0
//        while (elves.size > 1) {
//            val across = findAcross(i, elves.size)
//            // println("Elves: ${elves.size},  Current $i: -> elf ${elves[i]}, Across $across: -> elf ${elves[across]}")
//            elves.removeAt(across)
//            if (across > i) {
//                ++i
//            }
//            i = if (i >= elves.size) 0 else i
//        }
//        return elves.first()
//    }

//    private fun findAcross(cur: Int, size: Int): Int {
//        val step = (if (size % 2 == 1) size - 1 else size) / 2
//        return (cur + step) % size
//    }

    data class Node(val x: Int, var next: Node? = null, var prev: Node? = null) {
        fun delete() {
            prev!!.next = next
            next!!.prev = prev
        }
    }
}
