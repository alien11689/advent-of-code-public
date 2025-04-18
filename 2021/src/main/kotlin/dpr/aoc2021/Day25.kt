package dpr.aoc2021

import dpr.commons.Util
import dpr.commons.Point2D as Pos

object Day25 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/25/input.txt")
        println(part1(lines))
    }

    private fun part1(lines: List<String>): Any {
        var map = readInput(lines)
        val ySize = lines.size
        val xSize = lines[0].length
        var step = 0
        var prev = mutableMapOf<Pos, Char>()
        while (map != prev) {
            step++
            prev = map
            map = mutableMapOf()
            prev.filter { it.value == '>' }.forEach {
                val cur = it.key
                val right = it.key.rightModulo(xSize)
                if (right !in prev) {
                    map[right] = it.value
                } else {
                    map[cur] = it.value
                }
            }
            prev.filter { it.value == 'v' }.forEach {
                val cur = it.key
                val down = it.key.downModulo(ySize)
                val valueDownPrev = prev[down]
//                val valueDownCur = map[down]
                if (down in map) { // don't move
                    map[cur] = it.value
                } else if (down in prev && valueDownPrev == it.value) {
                    map[cur] = it.value
                } else {
                    map[down] = it.value
                }
            }
        }
        return step
    }

    private fun readInput(lines: List<String>): MutableMap<Pos, Char> {
        val map = mutableMapOf<Pos, Char>()
        lines.forEachIndexed { i, line ->
            line.forEachIndexed { j, c ->
                when (c) {
                    '>' -> map[Pos(j, i)] = c
                    'v' -> map[Pos(j, i)] = c
                    else -> Unit
                }
            }
        }
        return map
    }

//    fun printlnMap(map: Map<Pos, Char>, ySize: Int, xSize: Int) {
//        for (i in 0 until ySize) {
//            for (j in 0 until xSize) {
//                print(map[Pos(j, i)] ?: '.')
//            }
//            println()
//        }
//    }
}


