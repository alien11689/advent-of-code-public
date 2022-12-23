package pl.touk.dpr.aoc2022

object Day23 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/23/input.txt")
//        println("Part 1:")
//        println(part1(Util.getNotEmptyLinesFromFile("/23/test1.txt")))
        println(part1(lines))
//        println("Part 2:")
//        println(part2(Util.getNotEmptyLinesFromFile("/23/test1.txt")))
        println(part2(lines))
    }

    data class Elf(val x: Int, val y: Int) {
        fun proposeMove(curDirections: List<Direction>, elves: Set<Elf>): Elf {
            val neighbours = northRegion() + southRegion() + setOf(east(), west())
            if (neighbours.none { it in elves }) {
                return this
            }
            for (dir in curDirections) {
                when (dir) {
                    Direction.N -> if (northRegion().none { it in elves }) return north()
                    Direction.S -> if (southRegion().none { it in elves }) return south()
                    Direction.W -> if (westRegion().none { it in elves }) return west()
                    Direction.E -> if (eastRegion().none { it in elves }) return east()
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

    enum class Direction {
        N,
        S,
        W,
        E,
    }

    private fun part1(lines: List<String>): Any {
        var elves = parseElves(lines)
        val directions = Direction.values() + Direction.values()
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

    private fun nextGeneration(directions: Array<Direction>, round: Int, elves: Set<Elf>): Set<Elf> {
        val curDirection = directions.drop(round % 4).take(4)
//        println("Moving Order $curDirection")
        val possibleMoves = elves.associateWith { it.proposeMove(curDirection, elves) }
        val places = possibleMoves.values.fold(mutableMapOf<Elf, Int>()) { acc, elf ->
            acc[elf] = (acc[elf] ?: 0) + 1
            acc
        }
//        println("Moves ${possibleMoves.values.toSet()}")
        val newElves = possibleMoves.map { (curElf, move) ->
            if (places[move]!! == 1) move else curElf
        }.toSet()
        return newElves
    }

    private fun parseElves(lines: List<String>) = lines.flatMapIndexed { y, line ->
        line.mapIndexed { x, cur -> if (cur == '#') Elf(x, y) else null }.filterNotNull()
    }.toSet()

    private fun part2(lines: List<String>): Any {
        var elves = parseElves(lines)
        val directions = Direction.values() + Direction.values()
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

