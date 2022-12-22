package pl.touk.dpr.aoc2017

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
        var dir = Dir.R
        var last = 1
        mesh[cur] = last

        fun sumNeighbours(mesh: Map<Cur, Int>, cur: Cur): Int {
            return listOf(
                    cur + Dir.L,
                    cur + Dir.L + Dir.U,
                    cur + Dir.L + Dir.D,
                    cur + Dir.U,
                    cur + Dir.D,
                    cur + Dir.R,
                    cur + Dir.R + Dir.U,
                    cur + Dir.R + Dir.D)
                    .mapNotNull { mesh[it] }
                    .sum()
        }

        while (last <= input) {
            cur += dir
            last = sumNeighbours(mesh, cur)
            mesh[cur] = last
            if (mesh[cur + dir.nextDir()] == null) {
                dir = dir.nextDir()
            }
        }
        return last
    }

    data class Cur(val x: Int, val y: Int) {
        operator fun plus(d: Dir): Cur {
            return copy(x = x + d.x, y = y + d.y)
        }
    }

    enum class Dir(val x: Int, val y: Int) {
        R(1, 0),
        U(0, -1),
        L(-1, 0),
        D(0, 1);

        fun nextDir(): Dir = when (this) {
            R -> U
            U -> L
            L -> D
            D -> R
        }
    }
}
