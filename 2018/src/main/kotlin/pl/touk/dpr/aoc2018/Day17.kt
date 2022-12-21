package pl.touk.dpr.aoc2018

object Day17 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/17/input.txt")
        part1And2(input).forEach { println(it) }
    }

    private fun part1And2(input: List<String>): Collection<Int> {
        val claysSet = createClaysSet(input)
        val visitedDown = mutableSetOf<Pair<Int, Int>>()
        val filled = mutableSetOf<Pair<Int, Int>>()
        val drained = mutableSetOf<Pair<Int, Int>>()
        val y = claysSet.minByOrNull { it.second }!!.second - 1
        goDown(claysSet, filled, 500, y, drained, visitedDown)
        filled.addAll(drained)
//        printClays(claysSet, filled, drained)
        return listOf(filled.size, drained.size)
    }

    private fun createClaysSet(input: List<String>): MutableSet<Pair<Int, Int>> {
        val clays = readClays(input)
        val claysSet = mutableSetOf<Pair<Int, Int>>()
        clays.forEach {
            for (j in it.fromY..it.toY) {
                for (i in it.fromX..it.toX) {
                    claysSet.add(i to j)
                }
            }
        }
        return claysSet
    }

    private fun readClays(input: List<String>) = input.map { line ->
        val part = line.split(Regex("[, =]+"))
        val fromX: Int
        val toX: Int
        val fromY: Int
        val toY: Int
        val corrds1 = part[1].split("..").map { it.toInt() }
        val corrds2 = part[3].split("..").map { it.toInt() }
        if (part[0] == "x") {
            fromX = corrds1[0]
            toX = if (corrds1.size > 1) corrds1[1] else corrds1[0]
            fromY = corrds2[0]
            toY = if (corrds2.size > 1) corrds2[1] else corrds2[0]
        } else {
            fromY = corrds1[0]
            toY = if (corrds1.size > 1) corrds1[1] else corrds1[0]
            fromX = corrds2[0]
            toX = if (corrds2.size > 1) corrds2[1] else corrds2[0]
        }
        Clay(fromX, toX, fromY, toY)
    }

    data class Clay(val fromX: Int, val toX: Int, val fromY: Int, val toY: Int) {
        fun contains(x: Int, y: Int) = x in fromX..toX && y in fromY..toY
    }

    fun printClays(
            claysSet: Set<Pair<Int, Int>>,
            water: Set<Pair<Int, Int>>,
            drained: Set<Pair<Int, Int>>
    ) {
        for (j in claysSet.minByOrNull { it.second }!!.second..claysSet.maxByOrNull { it.second }!!.second) {
            for (i in claysSet.minByOrNull { it.first }!!.first..claysSet.maxByOrNull { it.first }!!.first) {
                val pos = i to j
                if (pos in drained) {
                    print('~')
                } else if (pos in water) {
                    print('|')
                } else if (pos in claysSet) {
                    print('#')
                } else {
                    print(' ')
                }
            }
            println()
        }
    }

    enum class ActionType {
        GoUp,
        Leak,
        Nop
    }

    data class Action(val type: ActionType, val curX: Int? = null)


    fun goDown(
            claysSet: Set<Pair<Int, Int>>,
            filled: MutableSet<Pair<Int, Int>>,
            initX: Int,
            initY: Int,
            drained: MutableSet<Pair<Int, Int>>,
            visitedDown: MutableSet<Pair<Int, Int>>
    ) {
        val x = initX
        var y = initY
        visitedDown.add(x to y)
        val maxY = claysSet.maxByOrNull { it.second }!!.second
        while (!claysSet.contains(x to y + 1) && y + 1 <= maxY) {
            ++y
            filled.add(x to y)
        }
        if (y + 1 > maxY) {
            return
        }
        var boundLeft = fillLeft(x, claysSet, y, filled, visitedDown)
        var boundRight = fillRight(x, claysSet, y, filled, visitedDown)
        while (boundLeft.type == ActionType.GoUp && boundRight.type == ActionType.GoUp) {
            drained.add(x to y)
            var i = x - 1
            while (!claysSet.contains(i to y)) {
                drained.add(i to y)
                --i
            }
            i = x + 1
            while (!claysSet.contains(i to y)) {
                drained.add(i to y)
                ++i
            }
            --y
            filled.add(x to y)
            boundLeft = fillLeft(x, claysSet, y, filled, visitedDown)
            boundRight = fillRight(x, claysSet, y, filled, visitedDown)
        }
        if (boundLeft.type == ActionType.Leak) {
            goDown(claysSet, filled, boundLeft.curX!!, y, drained, visitedDown)
        }
        if (boundRight.type == ActionType.Leak) {
            goDown(claysSet, filled, boundRight.curX!!, y, drained, visitedDown)
        }
    }

    private fun fillRight(
            x: Int,
            claysSet: Set<Pair<Int, Int>>,
            y: Int,
            filled: MutableSet<Pair<Int, Int>>,
            visitedDown: MutableSet<Pair<Int, Int>>
    ): Action {
        var cur = x
        while (true) {
            val nextX = cur + 1
            if (claysSet.contains(nextX to y)) {
                return Action(ActionType.GoUp)
            }
            val nextIsClay = claysSet.contains(nextX to y + 1)
            val nextIsFilled = filled.contains(nextX to y + 1)
            if (!nextIsClay && !nextIsFilled) {
                if (visitedDown.contains(nextX + 1 to y) || visitedDown.contains(nextX - 1 to y)) {
                    return Action(ActionType.Nop)
                }
                filled.add(nextX to y)
                return Action(ActionType.Leak, nextX)
            }
            ++cur
            filled.add(nextX to y)
            continue
        }
    }

    private fun fillLeft(
            x: Int,
            claysSet: Set<Pair<Int, Int>>,
            y: Int,
            filled: MutableSet<Pair<Int, Int>>,
            visitedDown: MutableSet<Pair<Int, Int>>
    ): Action {
        var cur = x
        while (true) {
            val prevX = cur - 1
            if (claysSet.contains(prevX to y)) {
                return Action(ActionType.GoUp)
            }
            val prevIsClay = claysSet.contains(prevX to y + 1)
            val prevIsFilled = filled.contains(prevX to y + 1)
            if (!prevIsClay && !prevIsFilled) {
                if (visitedDown.contains(prevX + 1 to y)) {
                    return Action(ActionType.Nop)
                }
                filled.add(prevX to y)
                return Action(ActionType.Leak, prevX)
            }
            --cur
            filled.add(prevX to y)
            continue
        }
    }

}
