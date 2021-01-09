package pl.touk.dpr.aoc2018

object Day13 {
    @JvmStatic
    fun main(args: Array<String>) {
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
            drivers.sortedBy { it.y * 10000 + it.x }.forEach {
                it.move(cells)
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
                        drivers.add(Driver(x, y, Direction.UP))
                        cells[y][x] = Type.PIPE
                    }
                    '>' -> {
                        drivers.add(Driver(x, y, Direction.RIGHT))
                        cells[y][x] = Type.MINUS
                    }
                    'v' -> {
                        drivers.add(Driver(x, y, Direction.DOWN))
                        cells[y][x] = Type.PIPE
                    }
                    '<' -> {
                        drivers.add(Driver(x, y, Direction.LEFT))
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
            var crashes = mutableListOf<Driver>()
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

    enum class Direction {
        UP {
            override fun turnLeft(): Direction {
                return LEFT
            }

            override fun turnRight(): Direction {
                return RIGHT
            }
        },
        DOWN {
            override fun turnLeft(): Direction {
                return RIGHT
            }

            override fun turnRight(): Direction {
                return LEFT
            }
        },
        LEFT {
            override fun turnLeft(): Direction {
                return Direction.DOWN
            }

            override fun turnRight(): Direction {
                return Direction.UP
            }
        },
        RIGHT {
            override fun turnLeft(): Direction {
                return Direction.UP
            }

            override fun turnRight(): Direction {
                return Direction.DOWN
            }
        };

        abstract fun turnLeft(): Direction

        abstract fun turnRight(): Direction
    }

    enum class DirOnCross {
        LEFT,
        STRAIGHT,
        RIGHT
    }

    data class Driver(var x: Int, var y: Int, var direction: Direction, var dirOnCross: DirOnCross = DirOnCross.LEFT) {
        fun move(cells: List<List<Type>>) {
            when (direction) {
                Direction.UP -> --y
                Direction.RIGHT -> ++x
                Direction.DOWN -> ++y
                Direction.LEFT -> --x
            }
            if (cells[y][x] == Type.SLASH) {
                when (direction) {
                    Direction.UP -> direction = Direction.RIGHT
                    Direction.RIGHT -> direction = Direction.UP
                    Direction.DOWN -> direction = Direction.LEFT
                    Direction.LEFT -> direction = Direction.DOWN
                }
            } else if (cells[y][x] == Type.BACKSHLASH) {
                when (direction) {
                    Direction.UP -> direction = Direction.LEFT
                    Direction.RIGHT -> direction = Direction.DOWN
                    Direction.DOWN -> direction = Direction.RIGHT
                    Direction.LEFT -> direction = Direction.UP
                }
            } else if (cells[y][x] == Type.CROSS) {
                when (dirOnCross) {
                    DirOnCross.LEFT -> {
                        direction = direction.turnLeft()
                        dirOnCross = DirOnCross.STRAIGHT
                    }
                    DirOnCross.STRAIGHT -> {
                        dirOnCross = DirOnCross.RIGHT
                    }
                    DirOnCross.RIGHT -> {
                        direction = direction.turnRight()
                        dirOnCross = DirOnCross.LEFT
                    }
                }
            }
        }

        fun sign(): String {
            return when (direction) {
                Direction.LEFT -> "<"
                Direction.UP -> "^"
                Direction.RIGHT -> ">"
                Direction.DOWN -> "v"
            }
        }
    }
}