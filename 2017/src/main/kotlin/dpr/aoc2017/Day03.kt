package dpr.aoc2017

import dpr.commons.Dir
import dpr.commons.Util
import dpr.commons.Point2D as Cur

object Day03 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = 277678
        println(part1(input))
        println(part2(input))
    }

    @JvmStatic
    fun part1(input: Int): Int {
        val mesh = mutableMapOf<Cur, Int>()
        var cur = Cur(0, 0)
        var dir = Dir.E
        var last = 1
        mesh[cur] = last

        while (last < input) {
            cur = cur.move(dir)
            last = ++last
            mesh[cur] = last
            if (mesh[cur.move(dir.turnLeft())] == null) {
                dir = dir.turnLeft()
            }
        }
        return cur.manhattan(Cur(0, 0))
    }

    @JvmStatic
    fun part2(input: Int): Int {
        val mesh = mutableMapOf<Cur, Int>()
        var cur = Cur(0, 0)
        var dir = Dir.E
        var last = 1
        mesh[cur] = last

        fun sumNeighbours(mesh: Map<Cur, Int>, cur: Cur): Int {
            return cur.adjacentPoints()
                .mapNotNull { mesh[it] }
                .sum()
        }

        while (last <= input) {
            cur = cur.move(dir)
            last = sumNeighbours(mesh, cur)
            mesh[cur] = last
            if (mesh[cur.move(dir.turnLeft())] == null) {
                dir = dir.turnLeft()
            }
        }
        return last
    }
}
