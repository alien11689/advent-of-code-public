package dpr.aoc2018

import dpr.commons.Dir
import dpr.commons.Util

object Day13 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/13/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val (cells, drivers) = readInput(input)

        var tick = 0
        while (true) {
            ++tick
//            println(tick)
//            println(drivers)
            drivers.sortedBy { it.y * 10000 + it.x }.forEach { driver ->
                driver.move(cells)
                val positions = mutableMapOf<Pair<Int, Int>, List<Driver>>()
                drivers.forEach { d -> positions[d.x to d.y] = (positions[d.x to d.y] ?: listOf()) + d }
                val crashes = positions.filter { it.value.size > 1 }.keys
//                println(positions)
                if (crashes.isNotEmpty()) {
//            printBoard(cells, drivers)
                    return crashes.map { "${it.first},${it.second}" }.first()
                }
            }
        }
    }

    private fun readInput(input: List<String>): Pair<List<MutableList<Type>>, MutableList<Driver>> {
        val cells = input.map { it.map { Type.EMPTY }.toMutableList() }
        val drivers = mutableListOf<Driver>()
        input.forEachIndexed { y, line ->
            line.forEachIndexed { x, v ->
                when (v) {
                    '/' -> cells[y][x] = Type.SLASH
                    '\\' -> cells[y][x] = Type.BACKSHLASH
                    '-' -> cells[y][x] = Type.MINUS
                    '|' -> cells[y][x] = Type.PIPE
                    '+' -> cells[y][x] = Type.CROSS
                    '^' -> {
                        drivers.add(Driver(x, y, Dir.N))
                        cells[y][x] = Type.PIPE
                    }

                    '>' -> {
                        drivers.add(Driver(x, y, Dir.E))
                        cells[y][x] = Type.MINUS
                    }

                    'v' -> {
                        drivers.add(Driver(x, y, Dir.S))
                        cells[y][x] = Type.PIPE
                    }

                    '<' -> {
                        drivers.add(Driver(x, y, Dir.W))
                        cells[y][x] = Type.MINUS
                    }

                    else -> {

                    }
                }
            }
        }
        return Pair(cells, drivers)
    }

    private fun part2(input: List<String>): Any {
        val (cells, drivers) = readInput(input)

        var tick = 0

        while (true) {
            ++tick
            val crashes = mutableListOf<Driver>()
            drivers.sortedBy { it.y * 10000 + it.x }.forEach {
                if (it !in crashes) {
                    it.move(cells)
                    val conflict = drivers.find { o -> it != o && o.x == it.x && o.y == it.y }
                    if (conflict != null) {
                        crashes.addAll(listOf(it, conflict))
                    }
                }
            }
            crashes.forEach { car ->
//        println("Crash on ${car.x} ${car.y}")
                drivers.remove(car)
            }
            if (drivers.size == 1) {
                val winner = drivers[0]
                return "${winner.x},${winner.y}"
            }
        }
    }

    enum class Type {
        SLASH, BACKSHLASH, MINUS, PIPE, CROSS, EMPTY
    }

    data class Cell(val type: Type) {
        override fun toString(): String {
            return when (type) {
                Type.SLASH -> '/'.toString()
                Type.BACKSHLASH -> '\\'.toString()
                Type.MINUS -> '-'.toString()
                Type.PIPE -> '|'.toString()
                Type.CROSS -> '+'.toString()
                Type.EMPTY -> ' '.toString()
            }
        }
    }

    enum class DirOnCross {
        LEFT,
        STRAIGHT,
        RIGHT
    }

    data class Driver(var x: Int, var y: Int, var dir: Dir, var dirOnCross: DirOnCross = DirOnCross.LEFT) {
        fun move(cells: List<List<Type>>) {
            when (dir) {
                Dir.N -> --y
                Dir.E -> ++x
                Dir.S -> ++y
                Dir.W -> --x
            }
            if (cells[y][x] == Type.SLASH) {
                dir = when (dir) {
                    Dir.N -> Dir.E
                    Dir.E -> Dir.N
                    Dir.S -> Dir.W
                    Dir.W -> Dir.S
                }
            } else if (cells[y][x] == Type.BACKSHLASH) {
                dir = when (dir) {
                    Dir.N -> Dir.W
                    Dir.E -> Dir.S
                    Dir.S -> Dir.E
                    Dir.W -> Dir.N
                }
            } else if (cells[y][x] == Type.CROSS) {
                when (dirOnCross) {
                    DirOnCross.LEFT -> {
                        dir = dir.turnLeft()
                        dirOnCross = DirOnCross.STRAIGHT
                    }

                    DirOnCross.STRAIGHT -> {
                        dirOnCross = DirOnCross.RIGHT
                    }

                    DirOnCross.RIGHT -> {
                        dir = dir.turnRight()
                        dirOnCross = DirOnCross.LEFT
                    }
                }
            }
        }
    }
}
