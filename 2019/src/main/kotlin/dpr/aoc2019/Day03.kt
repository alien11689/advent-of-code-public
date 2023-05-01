package dpr.aoc2019

import kotlin.math.absoluteValue

object Day03 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/03/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val a = getVisited1(input[0].split(','))
        val b = getVisited1(input[1].split(','))

        return a.intersect(b - (0 to 0)).minOf { it.first.absoluteValue + it.second.absoluteValue }
    }

    private fun part2(input: List<String>): Any {
        val a = getVisited2(input[0].split(','))
        val b = getVisited2(input[1].split(','))

        val intersections = (a.map { it[0] to it[1] }).intersect(b.map { it[0] to it[1] }.toSet()).toSet()

        return intersections.minOf { inter ->
            a.filter { cur -> cur[0] to cur[1] == inter }.minOf { it[2] } +
                    b.filter { cur -> cur[0] to cur[1] == inter }
                            .minOf { it[2] }
        }
    }

    private fun getVisited1(path: List<String>): Set<Pair<Int, Int>> {
        val mem = mutableSetOf<Pair<Int, Int>>()
        var cur = 0 to 0
        mem.add(cur)
        path.forEach { segment ->
            val dir = segment[0]
            val dist = segment.substring(1).toInt()
            when (dir) {
                'U' -> {
                    (cur.second..(cur.second + dist)).forEach {
                        mem.add(cur.first to it)
                    }
                    cur = cur.first to (cur.second + dist)
                }
                'D' -> {
                    (cur.second downTo (cur.second - dist)).forEach {
                        mem.add(cur.first to it)
                    }
                    cur = cur.first to (cur.second - dist)
                }
                'L' -> {
                    (cur.first downTo (cur.first - dist)).forEach {
                        mem.add(it to cur.second)
                    }
                    cur = (cur.first - dist) to cur.second
                }
                'R' -> {
                    (cur.first..(cur.first + dist)).forEach {
                        mem.add(it to cur.second)
                    }
                    cur = (cur.first + dist) to cur.second
                }
            }
        }
        return mem
    }

    private fun getVisited2(path: List<String>): Set<List<Int>> {
        val mem = mutableSetOf<List<Int>>()
        var cur = 0 to 0
        var step = 0
        path.forEach { segment ->
            val dir = segment[0]
            val dist = segment.substring(1).toInt()
            when (dir) {
                'U' -> {
                    ((cur.second + 1)..(cur.second + dist)).forEach {
                        mem.add(listOf(cur.first, it, ++step))
                    }
                    cur = cur.first to cur.second + dist
                }
                'D' -> {
                    ((cur.second - 1) downTo (cur.second - dist)).forEach {
                        mem.add(listOf(cur.first, it, ++step))
                    }
                    cur = cur.first to cur.second - dist
                }
                'L' -> {
                    ((cur.first - 1) downTo (cur.first - dist)).forEach {
                        mem.add(listOf(it, cur.second, ++step))
                    }
                    cur = cur.first - dist to cur.second
                }
                'R' -> {
                    ((cur.first + 1)..(cur.first + dist)).forEach {
                        mem.add(listOf(it, cur.second, ++step))
                    }
                    cur = cur.first + dist to cur.second
                }
            }
        }
        return mem
    }
}
