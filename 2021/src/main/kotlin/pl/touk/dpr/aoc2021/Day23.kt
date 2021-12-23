package pl.touk.dpr.aoc2021

import java.util.PriorityQueue

object Day23 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = Util.getNotEmptyLinesFromFile("/23/input.txt")
        Util.measureTimeAndPrint { part1(lines) }
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val (state, openSpace) = readInput1(lines)
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
                println(cur)
                return cur.score
            }
//            if (cur.path.map { it.name } == listOf('B', 'C', 'C', 'D') && cur.path.map { it.score } == listOf(40, 200, 400, 1000)) {
//                // błąd w y == 2 - D nie wyskakuje po przeniesieniu wyżej
//                println("Possible ${cur.path}")
//                println("Score ${cur.score}")
////                throw RuntimeException("Break")
//            }
//            println("And score is ${cur.score} ")
            mem.add(cur)
            val takenPos = cur.amipods.map { Pair(it.pos, it.name) }.toMap()
            for (amipod in cur.amipods) {
                if (amipod.pos.y == 1) {
                    val destX = State.DESTS[amipod.name]!!.map { it.x }.first()
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
                        val down1 = curPos.down()
                        if (down1 in takenPos) {
                            continue
                        }
                        ++moves
                        val down2 = down1.down()
                        if (down2 !in takenPos) {
                            val newAmipod = amipod.copy(pos = down2, score = amipod.score + scoreFor(amipod.name, moves + 1))
                            pq.offer(State(cur.amipods - amipod + newAmipod, cur.path + newAmipod))
                        } else if (takenPos[down2]!! == amipod.name) {
                            val newAmipod = amipod.copy(pos = down1, score = amipod.score + scoreFor(amipod.name, moves))
                            pq.offer(State(cur.amipods - amipod + newAmipod, cur.path + newAmipod))
                        }
                    }
                } else if (amipod.pos.y == 2) {
                    if (amipod.moved) {
                        continue
                    }
                    if (amipod.pos in State.DESTS[amipod.name]!!) {
                        val downChar = takenPos[amipod.pos.down()]
                        if (downChar == amipod.name) {
                            println("Continue")
                            // in right place
                            continue
                        }
                    }
                    val up = amipod.pos.up()
                    var left = up.left()
                    var moves = 2
                    while (left in openSpace && left !in takenPos) {
                        if (left.down() !in openSpace) {
                            val newAmipod = amipod.copy(moved = true, pos = left, score = amipod.score + scoreFor(amipod.name, moves))
                            pq.offer(State(cur.amipods - amipod + newAmipod, cur.path + newAmipod))
                        }
                        left = left.left()
                        moves += 1
                    }

                    var right = up.right()
                    moves = 2
                    while (right in openSpace && right !in takenPos) {
                        if (right.down() !in openSpace) {
                            val newAmipod = amipod.copy(moved = true, pos = right, score = amipod.score + scoreFor(amipod.name, moves))
                            pq.offer(State(cur.amipods - amipod + newAmipod, cur.path + newAmipod))
                        }
                        right = right.right()
                        moves += 1
                    }
                } else if (amipod.pos.y == 3) {
                    if (amipod.moved) {
//                        continue
                    } else if (amipod.pos in State.DESTS[amipod.name]!!) {
//                        println("$amipod is in good place")
                    } else if (amipod.pos.up() !in takenPos) {
                        val newAmipod = amipod.copy(pos = amipod.pos.up(), score = amipod.score + scoreFor(amipod.name, 1))
                        pq.offer(State(cur.amipods - amipod + newAmipod, cur.path + newAmipod))
                    }
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

    private fun readInput1(lines: List<String>): Pair<State, Set<Pos>> {
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
        return Pair(State(amipods), openSpace.toSet())
    }

    private fun part2(lines: List<String>): Any {

        return -1
    }

    data class State(val amipods: Set<Amipod>, val path: List<Amipod> = emptyList()) : Comparable<State> {
        val score = amipods.sumOf { it.score }

        override fun compareTo(other: State): Int {
            return score.compareTo(other.score)
        }

        fun isDone(): Boolean {
            val aa = amipods.filter { it.name == 'A' }.map { it.pos }.toSet()
            val bb = amipods.filter { it.name == 'B' }.map { it.pos }.toSet()
            val cc = amipods.filter { it.name == 'C' }.map { it.pos }.toSet()
            val dd = amipods.filter { it.name == 'D' }.map { it.pos }.toSet()
            return A_DEST == aa && B_DEST == bb && C_DEST == cc && D_DEST == dd
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
            val A_DEST = setOf(Pos(3, 2), Pos(3, 3))
            val B_DEST = setOf(Pos(5, 2), Pos(5, 3))
            val C_DEST = setOf(Pos(7, 2), Pos(7, 3))
            val D_DEST = setOf(Pos(9, 2), Pos(9, 3))
            val DESTS = mapOf(
                'A' to A_DEST,
                'B' to B_DEST,
                'C' to C_DEST,
                'D' to D_DEST,
            )
        }
    }

    data class Pos(val x: Int, val y: Int) {
        fun up(): Pos = copy(y = y - 1)
        fun down(): Pos = copy(y = y + 1)
        fun left(): Pos = copy(x = x - 1)
        fun right(): Pos = copy(x = x + 1)
    }

    data class Amipod(val name: Char, val moved: Boolean, val pos: Pos, val score: Int = 0)

}


