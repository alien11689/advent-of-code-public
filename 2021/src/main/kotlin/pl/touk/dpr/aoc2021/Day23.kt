package pl.touk.dpr.aoc2021

import java.util.PriorityQueue

object Day23 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        println(part1And2(Util.getNotEmptyLinesFromFile("/23/input.txt")))
        println(part1And2(Util.getNotEmptyLinesFromFile("/23/input_2.txt")))
    }

    private fun part1And2(lines: List<String>): Any {
        val (state, openSpace) = readInput(lines)
        return processInput(state, openSpace)
    }

    private fun processInput(state: State, openSpace: Set<Pos>): Int {
        val pq = PriorityQueue<State>()
        pq.offer(state)
        val mem = mutableSetOf<State>()
        while (!pq.isEmpty()) {
            val cur = pq.poll()
            if (cur in mem) {
                continue
            }
            if (cur.isDone()) {
                return cur.score
            }
            mem.add(cur)
            val takenPos = cur.amipods.associate { Pair(it.pos, it.name) }
            for (amipod in cur.amipods) {
                if (amipod.pos.y == 1) {
                    val destX = State.FULL_DEST[amipod.name]!!.first()
                    if (!cur.amipods.filter { it.pos.x == destX }.all { it.name == amipod.name }) {
                        continue
                    }
                    var moves = 0
                    var curPos = amipod.pos
                    while (destX < curPos.x) {
                        ++moves
                        curPos = curPos.left()
                        if (curPos in takenPos) {
                            break
                        }
                    }
                    while (destX > curPos.x) {
                        ++moves
                        curPos = curPos.right()
                        if (curPos in takenPos) {
                            break
                        }
                    }
                    if (curPos.x == destX) {
                        while (curPos.down() in openSpace && curPos.down() !in takenPos) {
                            ++moves
                            curPos = curPos.down()
                        }
                        val newAmipod = amipod.copy(pos = curPos)
                        pq.offer(State(cur.amipods - amipod + newAmipod, cur.score + scoreFor(amipod.name, moves), cur.path + newAmipod))
                    }
                } else {
                    if (amipod.moved) {
                        continue
                    }
                    if (cur.amipods.filter { it.pos.x == amipod.pos.x }.map { it.name }.toSet() == State.FULL_DEST[amipod.name]!!) {
                        continue
                    }
                    var pos = amipod.pos
                    var movesV = 0
                    while (pos.y != 1) {
                        pos = pos.up()
                        ++movesV
                        if (pos in takenPos) {
                            break
                        }
                    }
                    if (pos.y == 1) {
                        var left = pos.left()
                        var movesH = 1
                        while (left in openSpace && left !in takenPos) {
                            if (left.down() !in openSpace) {
                                val newAmipod = amipod.copy(moved = true, pos = left)
                                pq.offer(State(cur.amipods - amipod + newAmipod, cur.score + scoreFor(amipod.name, movesV + movesH), cur.path + newAmipod))
                            }
                            left = left.left()
                            movesH += 1
                        }
                        var right = pos.right()
                        movesH = 1
                        while (right in openSpace && right !in takenPos) {
                            if (right.down() !in openSpace) {
                                val newAmipod = amipod.copy(moved = true, pos = right)
                                pq.offer(State(cur.amipods - amipod + newAmipod, cur.score + scoreFor(amipod.name, movesV + movesH), cur.path + newAmipod))
                            }
                            right = right.right()
                            movesH += 1
                        }
                    }
                }
            }
        }
        return -1
    }

    private fun scoreFor(amipodName: Char, moves: Int): Int {
        val moveCost = when (amipodName) {
            'A' -> 1
            'B' -> 10
            'C' -> 100
            'D' -> 1000
            else -> throw RuntimeException()
        }
        return moves * moveCost
    }

    private fun readInput(lines: List<String>): Pair<State, Set<Pos>> {
        val openSpace = mutableSetOf<Pos>()
        val amipods = mutableSetOf<Amipod>()
        lines.forEachIndexed { i, line ->
            line.forEachIndexed { j, c ->
                when (c) {
                    '.' -> openSpace.add(Pos(j, i))
                    ' ', '#' -> {
                        // do nothing
                    }
                    else -> {
                        amipods.add(Amipod(c, false, Pos(j, i)))
                        openSpace.add(Pos(j, i))
                    }
                }
            }
        }
        return Pair(State(amipods, 0), openSpace.toSet())
    }

    data class State(val amipods: Set<Amipod>, val score: Int, val path: List<Amipod> = emptyList()) : Comparable<State> {
        override fun compareTo(other: State): Int {
            return score.compareTo(other.score)
        }

        fun isDone(): Boolean {
            if (amipods.any { it.pos.y == 1 }) {
                return false
            }
            val doneValues = amipods.groupBy({ it.name }) { it.pos.x }.mapValues { it.value.toSet() }
            return doneValues == FULL_DEST
        }

        override fun equals(other: Any?): Boolean {
            if (this === other) return true
            if (javaClass != other?.javaClass) return false

            other as State

            if (amipods != other.amipods) return false

            return true
        }

        override fun hashCode(): Int {
            return amipods.hashCode()
        }

        companion object {
            val FULL_DEST = mapOf('A' to setOf(3), 'B' to setOf(5), 'C' to setOf(7), 'D' to setOf(9))
        }
    }

    data class Pos(val x: Int, val y: Int) {
        fun up(): Pos = copy(y = y - 1)
        fun down(): Pos = copy(y = y + 1)
        fun left(): Pos = copy(x = x - 1)
        fun right(): Pos = copy(x = x + 1)
    }

    data class Amipod(val name: Char, val moved: Boolean, val pos: Pos)
}


