package dpr.aoc2022

import dpr.commons.Dir
import dpr.commons.Util

object Day23 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/23/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    data class Elf(val x: Int, val y: Int) {
        fun proposeMove(curDirections: List<Dir>, elves: Set<Elf>): Elf {
            val neighbours = northRegion() + southRegion() + setOf(east(), west())
            if (neighbours.none { it in elves }) {
                return this
            }
            for (dir in curDirections) {
                when (dir) {
                    Dir.N -> if (northRegion().none { it in elves }) return north()
                    Dir.S -> if (southRegion().none { it in elves }) return south()
                    Dir.W -> if (westRegion().none { it in elves }) return west()
                    Dir.E -> if (eastRegion().none { it in elves }) return east()
                }
            }
            return this
        }

        private fun northRegion(): Set<Elf> = setOf(
            Elf(x - 1, y - 1),
            copy(y = y - 1),
            Elf(x + 1, y - 1),
        )

        private fun southRegion(): Set<Elf> = setOf(
            Elf(x - 1, y + 1),
            copy(y = y + 1),
            Elf(x + 1, y + 1),
        )

        private fun eastRegion(): Set<Elf> = setOf(
            Elf(x + 1, y - 1),
            copy(x = x + 1),
            Elf(x + 1, y + 1),
        )

        private fun westRegion(): Set<Elf> = setOf(
            Elf(x - 1, y - 1),
            copy(x = x - 1),
            Elf(x - 1, y + 1),
        )

        private fun north() = copy(y = y - 1)
        private fun south() = copy(y = y + 1)
        private fun east() = copy(x = x + 1)
        private fun west() = copy(x = x - 1)
    }

    @JvmStatic fun part1(lines: List<String>): Int {
        var elves = parseElves(lines)
        val directions = listOf(Dir.N, Dir.S, Dir.W, Dir.E, Dir.N, Dir.S, Dir.W, Dir.E)
        repeat(10) { round ->
            elves = nextGeneration(directions, round, elves)
//            println("After round $round")
//            printElves(elves)
        }
        val minX = elves.minOf { it.x }
        val maxX = elves.maxOf { it.x }
        val minY = elves.minOf { it.y }
        val maxY = elves.maxOf { it.y }
//        println("x in $minX .. $maxX and y in $minY .. $maxY and we have ${elves.size} elves" )
        return (maxY - minY + 1) * (maxX - minX + 1) - elves.size
    }

    private fun nextGeneration(directions: List<Dir>, round: Int, elves: Set<Elf>): Set<Elf> {
        val curDirection = directions.drop(round % 4).take(4)
//        println("Moving Order $curDirection")
        return elves.groupBy { it.proposeMove(curDirection, elves) }
            .flatMap { (possibleMove, it) -> if (it.size == 1) listOf(possibleMove) else it }
            .toSet()
    }

    private fun parseElves(lines: List<String>) = lines.flatMapIndexed { y, line ->
        line.mapIndexed { x, cur -> if (cur == '#') Elf(x, y) else null }.filterNotNull()
    }.toSet()

    @JvmStatic fun part2(lines: List<String>): Int {
        var elves = parseElves(lines)
        val directions = listOf(Dir.N, Dir.S, Dir.W, Dir.E, Dir.N, Dir.S, Dir.W, Dir.E)
        var round = 0
        while (true) {
//            println("Running round ${round + 1}")
            val newElves = nextGeneration(directions, round, elves)
            if (newElves == elves) {
//                printElves(elves)
                return round + 1
            }
            elves = newElves
            ++round
        }
    }

//    private fun printElves(elves: Set<Elf>) {
//        val minX = elves.minOf { it.x }
//        val maxX = elves.maxOf { it.x }
//        val minY = elves.minOf { it.y }
//        val maxY = elves.maxOf { it.y }
//        for (y in minY..maxY) {
//            for (x in minX..maxX) {
//                print(if (Elf(x, y) in elves) "#" else ".")
//            }
//            println()
//        }
//    }
}

