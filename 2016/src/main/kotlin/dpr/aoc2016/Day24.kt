package dpr.aoc2016

import dpr.commons.Point2D
import dpr.commons.Util
import java.util.LinkedList

object Day24 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/24/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(maze: List<String>): Int {
        val init = find0(maze)
        val expected = findNumbers(maze)

        val memory = mutableSetOf<Stage>()
        val q = initQueue(init, memory)

        while (q.isNotEmpty()) {
            val step = q.poll()
//                    println "q: ${q.size()}, count: ${step.count}"
            if (step.stage.visited.size == expected) {
                return step.count
            }
            val neighbours = findNeighbours(step.stage, maze).filter { it !in memory }
            neighbours.forEach {
                memory.add(it)
                q.add(Step(count = step.count + 1, stage = it))
            }
        }
        throw RuntimeException("No solution")
    }

    private fun part2(maze: List<String>): Int {
        val init = find0(maze)
        val expected = findNumbers(maze)

        val toHomeMemory = mutableSetOf<Stage>()
        val memory = mutableSetOf<Stage>()
        val q = initQueue(init, memory)

        while (q.isNotEmpty()) {
            val step = q.poll()
//                    println "q: ${q.size()}, count: ${step.count}"
            if (step.stage.visited.size == expected && !step.toHome) {
                q.add(Step(count = step.count, stage = step.stage, toHome = true))
                toHomeMemory.add(step.stage)
            }
            if (step.toHome && maze[step.stage.p.y][step.stage.p.x] == '0') {
                return step.count
            }
            if (!step.toHome) {
                val neighbours = findNeighbours(step.stage, maze).filter { it !in memory }
                neighbours.forEach {
                    memory.add(it)
                    q.add(Step(count = step.count + 1, stage = it))
                }
            } else {
                val neighbours = findNeighbours(step.stage, maze).filter { it !in toHomeMemory }
                neighbours.forEach {
                    toHomeMemory.add(it)
                    q.add(Step(count = step.count + 1, stage = it))
                }
            }
        }
        throw RuntimeException("No solution")

    }

    private fun initQueue(init: Point2D, memory: MutableSet<Stage>): LinkedList<Step> {
        val q = LinkedList<Step>()

        val initStep = Step(count = 0, stage = Stage(visited = setOf('0'), p = init))
        q.add(initStep)
        memory.add(initStep.stage)
        return q
    }

    private fun findNeighbours(stage: Stage, maze: List<String>): List<Stage> {
        return stage.p.neighboursCross()
            .filter { it.x >= 0 && it.x < maze[0].length && it.y >= 0 && it.y < maze.size && maze[it.y][it.x] != '#' }
            .map {
                val cur = maze[it.y][it.x]
                Stage(
                    p = it,
                    visited = if (cur != '.') stage.visited + cur else stage.visited
                )
            }
    }

    data class Stage(val visited: Set<Char>, val p: Point2D)

    data class Step(val count: Int, val stage: Stage, val toHome: Boolean = false)

    private fun find0(maze: List<String>): Point2D {
        maze.forEachIndexed { yi, row ->
            row.forEachIndexed { xi, cell ->
                if (cell == '0') {
                    return Point2D(xi, yi)
                }
            }
        }
        throw RuntimeException("No 0")
    }

    private fun findNumbers(maze: List<String>): Int {
        var num = 0
        maze.forEach { row ->
            row.forEach { cell ->
                if (cell != '#' && cell != '.') {
                    ++num
                }
            }
        }
        return num
    }
}
