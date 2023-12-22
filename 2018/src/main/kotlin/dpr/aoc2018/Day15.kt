package dpr.aoc2018

import dpr.commons.Point2D
import dpr.commons.Util
import java.util.PriorityQueue


object Day15 {
    private const val MAX_HIT_POINTS = 200

    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/15/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val (board, initPlayers) = buildBoard(input)
        val players = initPlayers.toMutableList()
        val round = game(players, board)
        val sum = players.sumOf { it.hitPoints }
        return (round * sum)
    }

    private fun part2(input: List<String>): Any {
        val (board, initPlayers) = buildBoard(input)
        var minBound = 1
        var maxBound = MAX_HIT_POINTS
        var maxSum = -1
        while (minBound <= maxBound) {
//            println("Checking $minBound $maxBound")
            val i = (minBound + maxBound) / 2
            val players = initPlayers.map { if (it.type == PlayerType.E) it.copy(attackPower = i) else it.copy() }.toMutableList()
            try {
                val round = game(players, board, true)
                val sum = players.sumOf { it.hitPoints }
                maxSum = (round * sum)
                maxBound = i - 1
            } catch (e: ElfDied) {
                minBound = i + 1
            }
        }
        return maxSum
    }

    enum class PlayerType {
        E,
        G
    }

    data class Player(
        var p: Point2D,
        val type: PlayerType,
        var hitPoints: Int = MAX_HIT_POINTS,
        val attackPower: Int = 3,
        var moved: Boolean = true
    ) : Comparable<Player> {

        fun isDead(): Boolean {
            return hitPoints <= 0
        }

        override fun compareTo(other: Player): Int = p.compareTo(other.p)

        private fun neighbours(): List<Point2D> {
            return p.neighboursCross()
        }

        fun move(players: List<Player>, board: Set<Point2D>) {
            findNextMove(players, board).forEach { it.action() }
        }

        private fun findNextMove(players: List<Player>, board: Set<Point2D>): List<Action> {
            val enemies = neighbours().mapNotNull { n ->
                players.find { !it.isDead() && it.type != type && it.onPosition(n) }
            }
            if (enemies.isNotEmpty()) {
//                    println("Attack $enemy")
                return listOf(Attack(enemies.sorted().minBy { it.hitPoints }, this))
            }
//        println("I won't attack an enemy")
            val memory = mutableSetOf<Point2D>()
            memory.add(p)
            val queue = PriorityQueue<PositionWithDist> { o1, o2 ->
                if (o1.dist == o2.dist)
                    o1.pos.compareTo(o2.pos)
                else
                    o1.dist.compareTo(o2.dist)
            }
            queue.add(PositionWithDist(p))
            while (queue.isNotEmpty()) {
                try {
                    val cur = queue.poll()
                    val neighbours = cur.pos.neighboursCross()
                    neighbours.forEach { n ->
//                    println("Checking position $n")
                        if (n !in memory) {
                            if (n in board) {
                                memory.add(n)
                            } else {
                                val maybePlayer = players.find { !it.isDead() && it.onPosition(n) }
                                if (maybePlayer != null) {
//                                println("Neighbour $maybePlayer")
                                    if (maybePlayer.type != type) {
//                                    println("Moving... to ${cur.road[0]}")
                                        throw MoveOccurred(Move(this, cur.road[0]))
                                    }
                                } else {
                                    val next = PositionWithDist(n, cur.dist + 1, cur.road + n)
                                    queue.offer(next)
                                    memory.add(n)
                                }
                            }
                        }
                    }
                } catch (e: MoveOccurred) {
                    val es = e.move.newPosition.neighboursCross().mapNotNull { n ->
                        players.find { !it.isDead() && it.type != type && it.onPosition(n) }
                    }
                    if (es.isNotEmpty()) {
//                    println("Move and attack $enemy")
                        return listOf(e.move, Attack(es.sorted().minBy { it.hitPoints }, this))
                    }
                    return listOf(e.move)
                }
            }
            return listOf(Nop)
        }

        private fun onPosition(other: Point2D): Boolean {
            return this.p == other
        }
    }

    class MoveOccurred(val move: Move) : RuntimeException()

    interface Action {
        fun action()
    }

    data class Move(val p: Player, val newPosition: Point2D) : Action {
        override fun action() {
            p.p = newPosition
        }
    }

    data class Attack(val enemy: Player, val attacker: Player) : Action {
        override fun action() {
            enemy.hitPoints -= attacker.attackPower
        }
    }

    object Nop : Action {
        override fun action() {
        }
    }

    data class PositionWithDist(val pos: Point2D, val dist: Int = 0, val road: List<Point2D> = listOf())

    private fun buildBoard(lines: List<String>): Pair<Set<Point2D>, Set<Player>> {
        val walls = mutableSetOf<Point2D>()
        val players = mutableSetOf<Player>()
        lines.forEachIndexed { y, line ->
            line.forEachIndexed { x, c ->
                val p = Point2D(x, y)
                when (c) {
                    'G' -> players.add(Player(p, PlayerType.G))
                    'E' -> players.add(Player(p, PlayerType.E))
                    '#' -> walls.add(p)
                }
            }
        }
        return walls.toSet() to players.toSet()
    }

    private fun game(players: MutableList<Player>, board: Set<Point2D>, elfCannotDie: Boolean = false): Int {
        var round = 0
        try {
            while (true) {
//                println(round)
//                printBoard(players, board)
                players.forEach {
                    it.moved = false
                }
                val orderedPlayers = players.sorted()
                orderedPlayers.forEach { player ->
//                println("Player $player")
                    if (!player.isDead()) {
                        player.move(players, board)
                        player.moved = true
                        if (!players.any { !it.isDead() && it.type != player.type }) {
                            players.removeAll { it.isDead() }
                            if (players.all { it.moved }) {
                                ++round
                            }
                            throw End(round)
                        }
                    }
                }
                if (elfCannotDie && players.find { it.type == PlayerType.E && it.isDead() } != null) {
                    throw ElfDied()
                }
                players.removeAll { it.isDead() }
                ++round
            }
        } catch (end: End) {
//            println(round)
//            printBoard(players, board)
            return end.round
        }
    }

    class ElfDied : RuntimeException()

    class End(val round: Int) : RuntimeException()

//    fun printBoard(players: List<Player>, board: List<List<CellType>>) {
//        for (y in board.indices) {
//            for (x in board[y].indices) {
//                val p = players.find { it.onPosition(Position(x, y)) }
//                if (p != null) {
//                    print(p.type)
//                } else {
//                    print(if (board[y][x] == CellType.FREE) '.' else '#')
//                }
//            }
//            println()
//        }
//        players.sorted().forEach { println(it) }
//    }
}
