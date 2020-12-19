package pl.touk.dpr.aoc2020

import kotlin.math.absoluteValue

object Day12 {
    @JvmStatic
    fun main(args: Array<String>) {
//        val input = Util.getNotEmptyLinesFromFile("/12/test.txt")
        val input = Util.getNotEmptyLinesFromFile("/12/input.txt")
        part1(input)
        part2(input)
    }

    private fun part1(input: List<String>) {
        val ship = input.fold(Ship(0, 0, Dir.E, Waypoint(10, 1))) { acc, command ->
            acc.move(Command.of(command))
        }
        println(ship.manhattan())
    }

    private fun part2(input: List<String>) {
        val ship = input.fold(Ship(0, 0, Dir.E, Waypoint(10, 1))) { acc, command ->
            acc.move2(Command.of(command))
        }
        println(ship.manhattan())
    }

    enum class Dir(val x: Int) {
        E(0),
        N(1),
        W(2),
        S(3);

        fun left(num: Int): Dir {
            if (num == 0) {
                return this
            } else if (this == E) {
                return N.left(num - 90)
            } else if (this == N) {
                return W.left(num - 90)
            } else if (this == W) {
                return S.left(num - 90)
            } else if (this == S) {
                return E.left(num - 90)
            }
            throw RuntimeException("Wrong left")
        }

        fun right(num: Int): Dir {
            if (num == 0) {
                return this
            } else if (this == E) {
                return S.right(num - 90)
            } else if (this == S) {
                return W.right(num - 90)
            } else if (this == W) {
                return N.right(num - 90)
            } else if (this == N) {
                return E.right(num - 90)
            }
            throw RuntimeException("Wrong left")
        }

    }

    data class Command(val sign: Char, val num: Int) {
        companion object {
            fun of(s: String) = Command(s.first(), s.substring(1).toInt())
        }
    }

    data class Ship(val x: Int, val y: Int, val dir: Dir, val waypoint: Waypoint) {
        fun manhattan(): Int {
            return x.absoluteValue + y.absoluteValue
        }

        fun move(command: Command): Ship {
            when (command.sign) {
                'N' -> return copy(y = y + command.num)
                'S' -> return copy(y = y - command.num)
                'E' -> return copy(x = x + command.num)
                'W' -> return copy(x = x - command.num)
                'F' -> return move(Command(dir.name.first(), command.num))
                'L' -> return copy(dir = dir.left(command.num))
                'R' -> return copy(dir = dir.right(command.num))
            }
            throw RuntimeException("Unknown command")
        }

        fun move2(command: Command): Ship {
            when (command.sign) {
                'N' -> return copy(waypoint = waypoint.copy(y = waypoint.y + command.num))
                'S' -> return copy(waypoint = waypoint.copy(y = waypoint.y - command.num))
                'E' -> return copy(waypoint = waypoint.copy(x = waypoint.x + command.num))
                'W' -> return copy(waypoint = waypoint.copy(x = waypoint.x - command.num))
                'F' -> return copy(x = x + command.num * waypoint.x, y = y + command.num * waypoint.y)
                'L' -> return copy(waypoint = waypoint.left(command.num))
                'R' -> return copy(waypoint = waypoint.right(command.num))
            }
            throw RuntimeException("Unknown command")
        }

    }

    data class Waypoint(val x: Int, val y: Int) {
        fun left(num: Int): Waypoint {
            if (num == 0) {
                return this
            }
            return Waypoint(-y, x).left(num - 90)
        }

        fun right(num: Int): Waypoint {
            if (num == 0) {
                return this
            }
            return Waypoint(y, -x).right(num - 90)
        }
    }
}
