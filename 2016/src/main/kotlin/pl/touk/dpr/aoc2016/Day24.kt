package pl.touk.dpr.aoc2016

import java.util.LinkedList

object Day24 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/24/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val maze = input
        val (initX, initY) = find0(maze)
        val expected = findNumbers(maze)

        val memory = mutableSetOf<Stage>()
        val q = LinkedList<Step>()

        val initStep = Step(count = 0, stage = Stage(visited = setOf('0'), x = initX, y = initY))
        q.add(initStep)
        memory.add(initStep.stage)

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

    private fun part2(input: List<String>): Any {
        val maze = input
        val (initX, initY) = find0(maze)
        val expected = findNumbers(maze)

        val memory = mutableSetOf<Stage>()
        val toHomeMemory = mutableSetOf<Stage>()
        val q = LinkedList<Step>()

        val initStep = Step(count = 0, stage = Stage(visited = setOf('0'), x = initX, y = initY))
        q.add(initStep)
        memory.add(initStep.stage)

        while (q.isNotEmpty()) {
            val step = q.poll()
//                    println "q: ${q.size()}, count: ${step.count}"
            if (step.stage.visited.size == expected && !step.toHome) {
                q.add(Step(count = step.count, stage = step.stage, toHome = true))
                toHomeMemory.add(step.stage)
            }
            if (step.toHome && maze[step.stage.y][step.stage.x] == '0') {
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

    private fun findNeighbours(stage: Stage, maze: List<String>): List<Stage> {
        return listOf(
                listOf(stage.x + 1, stage.y),
                listOf(stage.x - 1, stage.y),
                listOf(stage.x, stage.y + 1),
                listOf(stage.x, stage.y - 1),
        )
                .filter { it[0] >= 0 && it[0] < maze[0].length && it[1] >= 0 && it[1] < maze.size && maze[it[1]][it[0]] != '#' }
                .map {
                    val cur = maze[it[1]][it[0]]
                    Stage(
                            x = it[0],
                            y = it[1],
                            visited = if (cur != '.') stage.visited + cur else stage.visited
                    )
                }
    }

    data class Stage(val visited: Set<Char>, val x: Int, val y: Int)

    data class Step(val count: Int, val stage: Stage, val toHome: Boolean = false)

    private fun find0(maze: List<String>): Pair<Int, Int> {
        maze.forEachIndexed { yi, row ->
            row.forEachIndexed { xi, cell ->
                if (cell == '0') {
                    return Pair(xi, yi)
                }
            }
        }
        throw RuntimeException("No 0")
    }

    private fun findNumbers(maze: List<String>): Int {
        var num = 0
        maze.forEachIndexed { yi, row ->
            row.forEachIndexed { xi, cell ->
                if (cell != '#' && cell != '.') {
                    ++num
                }
            }
        }
        return num
    }
}
