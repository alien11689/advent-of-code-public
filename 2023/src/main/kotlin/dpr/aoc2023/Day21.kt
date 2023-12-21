package dpr.aoc2023

import dpr.commons.Point2D
import dpr.commons.Util

object Day21 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/21/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/21/test1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val (garden, start) = readGarden(lines)
        var i = 0
        var prev = emptySet<Point2D>()
        var newlyAdded = setOf(start)
        var available = setOf(start)
        while (i < 64) {
            newlyAdded = newlyAdded.flatMap { it.neighboursCross() }.filter { garden[it] == '.' && it !in prev }.toSet()
            val prevBck = prev
            prev = available
            available = prevBck + newlyAdded
            ++i
        }
        return available.size
    }

    private fun readGarden(lines: List<String>): Pair<MutableMap<Point2D, Char>, Point2D> {
        val garden = mutableMapOf<Point2D, Char>()
        var start = Point2D(0, 0)
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                val cur = Point2D(x, y)
                when (c) {
                    'S' -> {
                        garden[cur] = '.'
                        start = cur
                    }

                    else -> {
                        garden[cur] = c
                    }
                }
            }
        }
        return Pair(garden, start)
    }

    private fun part2(lines: List<String>): Any {
        val (garden, start) = readGarden(lines)
        var i = 0
        var prevSize = 0L
        var prevNewlyAdded = emptySet<Point2D>()
        var newlyAdded = setOf(start)
        var curSize = 1L
        val limit = 26_501_365
        val xSize = lines[0].length
        val ySize = lines.size
        var interestingI = 65
        val mem = mutableMapOf<Int, Long>()
        val softLimit = xSize / 2 + xSize * 4
        while (i < limit) {
            val curSizeBck = curSize
            val prevNewlyAddedBck = prevNewlyAdded
            prevNewlyAdded = newlyAdded
            newlyAdded = newlyAdded.flatMap { it.neighboursCross() }
                .filter { garden[it.mod(xSize, ySize)] == '.' && it !in prevNewlyAddedBck }
                .toSet()
            val newlyAddedSize = newlyAdded.size
            curSize = prevSize + newlyAddedSize
            prevSize = curSizeBck
            ++i
            if (i == interestingI) {
                if (i <= softLimit) {
//                    println("Iter $i: curSize -> $curSize")
                    mem[interestingI] = curSize
                    interestingI += xSize
                } else {
                    while (i <= limit) {
                        curSize = mem[i - 2 * xSize]!! + 2 * (mem[i - xSize]!! - mem[i - 3 * xSize]!!) - (mem[i - 2 * xSize]!! - mem[i - 4 * xSize]!!)
                        mem[i] = curSize
//                        println("Iter $i: $curSize")
                        i += xSize
                    }
                }
            }

        }
        return curSize
    }
}
/*
Iter 65: 1 increment -> 260, curSize -> 3884
Iter 196: 10 increment -> 793, curSize -> 34564
Iter 327: 18 increment -> 1326, curSize -> 95816
Iter 458: 26 increment -> 1859, curSize -> 187640
Iter 589: 34 increment -> 2392, curSize -> 310036
Iter 720: 42 increment -> 2925, curSize -> 463004
Iter 851: 50 increment -> 3458, curSize -> 646544
Iter 982: 58 increment -> 3991, curSize -> 860656
Iter 1113: 66 increment -> 4524, curSize -> 1105340
Iter 1244: 74 increment -> 5057, curSize -> 1380596
Iter 1375: 82 increment -> 5590, curSize -> 1686424
Iter 1506: 90 increment -> 6123, curSize -> 2022824
Iter 1637: 98 increment -> 6656, curSize -> 2389796
Iter 1768: 106 increment -> 7189, curSize -> 2787340
Iter 1899: 114 increment -> 7722, curSize -> 3215456
Iter 2030: 122 increment -> 8255, curSize -> 3674144
Iter 2161: 130 increment -> 8788, curSize -> 4163404
Iter 2292: 138 increment -> 9321, curSize -> 4683236
Iter 2423: 146 increment -> 9854, curSize -> 5233640
Iter 2554: 154 increment -> 10387, curSize -> 5814616
Iter 2685: 162 increment -> 10920, curSize -> 6426164
Iter 2816: 170 increment -> 11453, curSize -> 7068284
Iter 2947: 178 increment -> 11986, curSize -> 7740976
Iter 3078: 186 increment -> 12519, curSize -> 8444240
Iter 3209: 194 increment -> 13052, curSize -> 9178076
Iter 3340: 202 increment -> 13585, curSize -> 9942484
Iter 3471: 210 increment -> 14118, curSize -> 10737464
Iter 3602: 218 increment -> 14651, curSize -> 11563016
Iter 3733: 226 increment -> 15184, curSize -> 12419140
Iter 3864: 234 increment -> 15717, curSize -> 13305836
Iter 3995: 242 increment -> 16250, curSize -> 14223104
Iter 4126: 250 increment -> 16783, curSize -> 15170944
Iter 4257: 258 increment -> 17316, curSize -> 16149356
Iter 4388: 266 increment -> 17849, curSize -> 17158340
Iter 4519: 274 increment -> 18382, curSize -> 18197896
Iter 4650: 282 increment -> 18915, curSize -> 19268024
Iter 4781: 290 increment -> 19448, curSize -> 20368724
Iter 4912: 298 increment -> 19981, curSize -> 21499996

observations:
- my input has straight empty lines in all the main directions so
- after 65 iterations we go outside of the garden are increasing by first SNWE
- next after 131 iterations we are entering the next garden by SNWE
- target limit is 65 * 202300 * 131
- there should be function of increase between each 131 or each 2*131 = 262
- size[0] - size[-2] == size[-1] - size[-3]
*/
