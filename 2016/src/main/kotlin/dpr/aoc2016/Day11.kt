package dpr.aoc2016

import dpr.commons.Util
import java.util.LinkedList
import java.util.Queue

object Day11 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        println(part1())
        println(part2())
    }

    private fun part2() = solve(
        listOf(
            setOf(1, 2, -2, 3, 4, -4, 5, -5, 6, -6, 7, -7),
            setOf(-1, -3),
            setOf(),
            setOf()
        )
    )

    private fun part1() = solve(
        listOf(
            setOf(1, 2, -2, 3, 4, -4, 5, -5),
            setOf(-1, -3),
            setOf(),
            setOf()
        )
    )

    private fun solve(input: List<Set<Int>>): Any {
        val mem = mutableSetOf<Any>()
        val q: Queue<Move> = LinkedList()
        val init = Move(0, Stage(0, input))
        q.add(init)
        while (q.isNotEmpty()) {
            val cur = q.poll()
            if (cur.stage.e == 3 && cur.stage.floors.take(3).all { it.isEmpty() }) {
                return cur.stepCount
            }
            val stepCount = cur.stepCount + 1
            generateMoves(cur.stage.e, cur.stage.floors, mem)
                .forEach {
                    q.offer(Move(stepCount, it))
                }
        }
        throw RuntimeException()
    }

    private fun generateMoves(e: Int, floors: List<Set<Int>>, mem: MutableSet<Any>): List<Stage> {
        val currentFloor = floors[e]
        val combinations = combinationsOf2(currentFloor)
        return combinations
            .filter { isValid(currentFloor - it) }
            .flatMap { m ->
                listOf(e + 1, e - 1).filter { it in 0..3 }.map { newE ->
                    val newFloors = floors.toMutableList()
                    newFloors[newE] = newFloors[newE] + m
                    newFloors[e] = newFloors[e] - m
                    if (isValid(newFloors[newE])) {
                        val st = Stage(newE, newFloors)
                        val toMem = genStates(st)
                        if (toMem in mem) {
                            null
                        } else {
                            mem.add(toMem)
                            st
                        }
                    } else {
                        null
                    }
                }
            }.filterNotNull()
    }

    private fun genStates(stage: Stage): Pair<Int, List<List<Int>>> {
        return Pair(
            stage.e,
            stage.floors.map { floor ->
                val gs = floor.filter { it > 0 }.toSet()
                val ms = floor.filter { it < 0 }.map { -it }.toSet()
                listOf(
                    (gs - ms).size,
                    (ms - gs).size,
                    gs.intersect(ms).size
                )
            })
    }

    private fun isValid(floor: Set<Int>): Boolean {
        val generators = floor.filter { it > 0 }
        return generators.isEmpty() || floor.filter { it < 0 }.all { m ->
            generators.any { g -> m == -g }
        }
    }

    private fun combinationsOf2(currentFloor: Set<Int>): Set<Set<Int>> {
        val res = mutableSetOf<Set<Int>>()
        currentFloor.forEach { a ->
            currentFloor.forEach { b ->
                res.add(setOf(a, b))
            }
        }
        return res
    }

    data class Move(val stepCount: Int, val stage: Stage)

    data class Stage(val e: Int, val floors: List<Set<Int>>)
}
