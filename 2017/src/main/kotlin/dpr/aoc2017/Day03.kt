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

    private fun part1(input: Int): Any {
//        println("Search for $input")
// left 2, 11, 28,
// diff 9, 17, 24
// side 3, 5, 7,
// next side = s + 2
// next diff = (s - 1)/2 + 3*s + 3 + (s + 2 + 1)/2
// s = 3
        var index = 1
        var lowerRight = 1
        while (lowerRight <= input) {
            index += 2
            lowerRight = index * index
        }
//        println("Lower right")
//        println(index)
//        println(lowerRight)
//        println("Lower left")
//        val loweLeft = (lowerRight - index + 1)
//        println(loweLeft)
//        println("Lower left is lower than search: ${loweLeft < input}")
        val halfSide = ((index + 1) / 2)
//        println("Half side = $halfSide")
        val distSide = halfSide - 1
//        println("Dist side $distSide")
        val middleSide = (lowerRight - distSide)
//        println("Middle side $middleSide")
        val leftOrRight = input - middleSide
        //        println("Distance $manhattan")
        return distSide + leftOrRight
    }

    private fun part2(input: Int): Any {
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
