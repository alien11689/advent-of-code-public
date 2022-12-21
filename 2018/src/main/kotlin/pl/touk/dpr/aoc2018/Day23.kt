package pl.touk.dpr.aoc2018

import kotlin.math.abs

object Day23 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/23/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val drones = readInput(input)

        val max = drones.maxByOrNull { it.range }!!

        return drones.count { it.manhattan(max) <= max.range }
    }

    private fun readInput(input: List<String>): List<Drone> {
        val drones = input.map { it.split(Regex("[<>,=]+")) }.map {
            Drone(it[1].toInt(), it[2].toInt(), it[3].toInt(), it[5].toInt())
        }
        return drones
    }

    private fun part2(input: List<String>): Any {
        val drones = readInput(input)
        val ZERO = Point(0, 0, 0)

        var minX = drones.minByOrNull { it.x }!!.x
        var maxX = drones.maxByOrNull { it.x }!!.x
        var minY = drones.minByOrNull { it.y }!!.y
        var maxY = drones.maxByOrNull { it.y }!!.y
        var minZ = drones.minByOrNull { it.z }!!.z
        var maxZ = drones.maxByOrNull { it.z }!!.z

        var distX = (maxX - minX) / 2
        var distY = (maxY - minY) / 2
        var distZ = (maxZ - minZ) / 2

        var best = Point(Int.MAX_VALUE, Int.MAX_VALUE, Int.MAX_VALUE)
        var maxFDest = 0
        var minDist = 1000000000

        var iter = 0
        while (distX > 1) {
            ++iter
//    println("Iter $iter - dists ($distX, $distY, $distZ)")
            for (x in minX..maxX step distX) {
                for (y in minY..maxY step distY) {
                    for (z in minZ..maxZ step distZ) {
                        val p = Point(x, y, z)
                        val fdest = p.fDest(drones)
                        val dist = p.manhattan(ZERO)
                        if (fdest > maxFDest || fdest == maxFDest && dist < minDist) {
                            maxFDest = fdest
                            best = p
                            minDist = dist
//                    println("New best $best, count $maxFDest, distTo0 $minDist")
                        }
                    }
                }
            }
            minX = best.x - distX
            maxX = best.x + distX
            minY = best.y - distY
            maxY = best.y + distY
            minZ = best.z - distZ
            maxZ = best.z + distZ
            distX /= 2
            distY /= 2
            distZ /= 2
        }
        return minDist

    }

    data class Drone(val x: Int, val y: Int, val z: Int, val range: Int) {
        fun manhattan(o: Drone): Int {
            return abs(x - o.x) + abs(y - o.y) + abs(z - o.z)
        }

        fun manhattan(o: Point): Int {
            return abs(x - o.x) + abs(y - o.y) + abs(z - o.z)
        }
    }

    data class Point(val x: Int, val y: Int, val z: Int, var fdest: Int = -1) {

        fun fDest(drones: List<Drone>): Int {
            var count = 0
            for (i in drones.indices) {
                count += if (drones[i].manhattan(this) <= drones[i].range) 1 else 0
            }
            fdest = count
            return count
        }

        fun manhattan(o: Point): Int {
            return abs(x - o.x) + abs(y - o.y) + abs(z - o.z)
        }
    }
}
