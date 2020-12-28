package pl.touk.dpr.aoc2016

object Day19 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = 3017957
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: Int): Any {
        val elvesWithoutPresent = mutableSetOf<Int>()
        var i = 0
        while (elvesWithoutPresent.size < input - 1) {
            val next = findNextWith(i, input, elvesWithoutPresent)
            elvesWithoutPresent.add(next)
            i = findNextWith(i, input, elvesWithoutPresent)
        }
        return i + 1
    }

    private fun findNextWith(cur: Int, all: Int, ewp: Set<Int>): Int {
        var prev = cur
        while (true) {
            val next = (prev + 1) % all
            if (next == cur || next !in ewp) {
                return next
            }
            prev = next
        }
    }

    private fun part2(input: Int): Any {
        val elves = ArrayList<Int>()
        elves.addAll((1..input))
        var i = 0
        while (elves.size > 1) {
            val across = findAcross(i, elves.size)
//            println("Elves: ${elves.size},  Current $i: -> elf ${elves[i]}, Across $across: -> elf ${elves[across]}")
            elves.removeAt(across)
            if (across > i) {
                ++i
            }
            i = if (i >= elves.size) 0 else i
        }
        return elves.first()
    }

    private fun findAcross(cur: Int, size: Int): Int {
        val step = (if (size % 2 == 1) size - 1 else size) / 2
        return (cur + step) % size
    }

    data class Node(val x:Int, val next: Node? = null, val prev: Node? = null)
}