package pl.touk.dpr.aoc2021

import java.util.PriorityQueue

object Day23 {
    @JvmStatic
    fun main(args: Array<String>) {
        val inputFile = "/23/input"
        println(part1(Util.getNotEmptyLinesFromFile(inputFile + ".txt")))
        println(part1(Util.getNotEmptyLinesFromFile(inputFile + "_2.txt")))
    }

    private fun part1(lines: List<String>): Any {
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
            //            println("Pq size is ${pq.size} and score is ${cur.score}")
            if (cur.isDone()) {
                //                println(cur)
                return cur.score
            }
            mem.add(cur)
            val takenPos = cur.amipods.map { Pair(it.pos, it.name) }.toMap()
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
                    //                    if (amipod.pos.y == 2) {
                    ////                    if (amipod.pos.x in State.FULL_DEST[amipod.name]!!) {
                    ////                        val downChar = takenPos[amipod.pos.down()]
                    ////                        if (downChar == amipod.name) {
                    ////                            println("Continue")
                    ////                            // in right place
                    ////                            continue
                    ////                        }
                    ////                    }
                    //                        val up = amipod.pos.up()
                    //                        var left = up.left()
                    //                        var moves = 2
                    //                        while (left in openSpace && left !in takenPos) {
                    //                            if (left.down() !in openSpace) {
                    //                                val newAmipod = amipod.copy(moved = true, pos = left)
                    //                                pq.offer(State(cur.amipods - amipod + newAmipod, cur.score + scoreFor(amipod.name, moves), cur.path + newAmipod))
                    //                            }
                    //                            left = left.left()
                    //                            moves += 1
                    //                        }
                    //
                    //                        var right = up.right()
                    //                        moves = 2
                    //                        while (right in openSpace && right !in takenPos) {
                    //                            if (right.down() !in openSpace) {
                    //                                val newAmipod = amipod.copy(moved = true, pos = right)
                    //                                pq.offer(State(cur.amipods - amipod + newAmipod, cur.score + scoreFor(amipod.name, moves), cur.path + newAmipod))
                    //                            }
                    //                            right = right.right()
                    //                            moves += 1
                    //                        }
                    //                    } else if (amipod.pos.y == 3) {
                    //                        if (amipod.moved) {
                    ////                        continue
                    //                        } else if (amipod.pos.x in State.FULL_DEST[amipod.name]!!) {
                    ////                        println("$amipod is in good place")
                    //                        } else if (amipod.pos.up() !in takenPos) {
                    //                            val newAmipod = amipod.copy(pos = amipod.pos.up())
                    //                            pq.offer(State(cur.amipods - amipod + newAmipod, cur.score + scoreFor(amipod.name, 1), cur.path + newAmipod))
                    //                        }
                    //                    }
                }
            }
        }
        return -1
    }

    fun scoreFor(amipodName: Char, moves: Int): Int {
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
                if (c == '.') {
                    openSpace.add(Pos(j, i))
                } else if (c == ' ' || c == '#') {
                    // do nothing
                } else {
                    amipods.add(Amipod(c, false, Pos(j, i)))
                    openSpace.add(Pos(j, i))
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
//            println("Done values $doneValues expected to be $FULL_DEST")
            return doneValues == FULL_DEST
//////            amipods.groupBy { it.name }.mapValues { it.value. }
//            val aa = amipods.filter { it.name == 'A' }.map { it.pos }.toSet()
//            val bb = amipods.filter { it.name == 'B' }.map { it.pos }.toSet()
//            val cc = amipods.filter { it.name == 'C' }.map { it.pos }.toSet()
//            val dd = amipods.filter { it.name == 'D' }.map { it.pos }.toSet()
//            return A_DEST == aa && B_DEST == bb && C_DEST == cc && D_DEST == dd
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

    private fun part2(lines: List<String>): Any {
        val (state, openSpace) = readInput(lines)
        println(state)
        println(openSpace)
        return -1
    }
}


