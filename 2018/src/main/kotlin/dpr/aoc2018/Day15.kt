package dpr.aoc2018

import java.util.PriorityQueue

object Day15 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/15/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val board = buildBoard(input)
        val players = buildPlayers(input).toMutableList()
        val round = game(players, board)
        val sum = players.sumOf { it.hitPoints }
        return (round * sum)
    }

    private fun part2(input: List<String>): Any {
        val board = buildBoard(input)
        var minBound = 0
        var maxBound = Integer.MAX_VALUE
        var maxSum = -1
        while (minBound <= maxBound) {
//            println("Checking $minBound $maxBound")
            val i = (minBound + maxBound) / 2
            val players = buildPlayers(input, i).toMutableList()
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

    data class Position(val x: Int, val y: Int) : Comparable<Position> {

        fun neighbours(): List<Position> {
            return listOf(
                    Position(x + 1, y),
                    Position(x - 1, y),
                    Position(x, y - 1),
                    Position(x, y + 1),
            )
        }

        override fun compareTo(other: Position): Int {
            val dy = y - other.y
            return if (dy != 0) dy else x - other.x
        }
    }

    data class Player(
            var x: Int,
            var y: Int,
            val type: PlayerType,
            var hitPoints: Int = 200,
            val attackPower: Int = 3,
            var moved: Boolean = true
    ) : Comparable<Player> {

        fun isDead(): Boolean {
            return hitPoints <= 0
        }

        override fun compareTo(other: Player): Int {
            val dy = y - other.y
            return if (dy != 0) {
                dy
            } else {
                x - other.x
            }
        }

        private fun neighbours(): List<Position> {
            return Position(x, y).neighbours()
        }

        fun move(players: List<Player>, board: List<List<CellType>>) {
            findNextMove(players, board).forEach { it.action() }
        }

        private fun findNextMove(players: List<Player>, board: List<List<CellType>>): List<Action> {
            val enemies = neighbours().mapNotNull { n ->
                players.find { !it.isDead() && it.type != type && it.onPosition(n) }
            }
            if (enemies.isNotEmpty()) {
//                    println("Attack $enemy")
                return listOf(Attack(enemies.sorted().minBy { it.hitPoints }, this))
            }
//        println("I won't attack an enemy")
            val memory = mutableSetOf<Position>()
            memory.add(Position(x, y))
            val queue = PriorityQueue<PositionWithDist> { o1, o2 ->
                if (o1.dist == o2.dist)
                    o1.pos.compareTo(o2.pos)
                else
                    o1.dist.compareTo(o2.dist)
            }
            queue.add(PositionWithDist(Position(x, y)))
            while (queue.isNotEmpty()) {
                try {
                    val cur = queue.poll()
                    val neighbours = cur.pos.neighbours()
                    neighbours.forEach { n ->
//                    println("Checking position $n")
                        if (n !in memory) {
                            if (board[n.y][n.x] == CellType.CLOSED) {
                                memory.add(n)
                            } else {
                                val maybePlayer = players.find { !it.isDead() && it.onPosition(n) }
                                if (maybePlayer != null) {
//                                println("Neighbour $maybePlayer")
                                    if (maybePlayer.type != type) {
//                                    println("Moving... to ${cur.road[0]}")
                                        throw MoveOccured(Move(this, cur.road[0]))
                                    }
                                } else {
                                    val next = PositionWithDist(n, cur.dist + 1, cur.road + n)
                                    queue.offer(next)
                                    memory.add(n)
                                }
                            }
                        }
                    }
                } catch (e: MoveOccured) {
                    val es = e.move.newPosition.neighbours().mapNotNull { n ->
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

        private fun onPosition(p: Position): Boolean {
            return x == p.x && y == p.y
        }
    }

    class MoveOccured(val move: Move) : RuntimeException()

    interface Action {
        fun action()
    }

    data class Move(val p: Player, val newPosition: Position) : Action {
        override fun action() {
            p.x = newPosition.x
            p.y = newPosition.y
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

    data class PositionWithDist(val pos: Position, val dist: Int = 0, val road: List<Position> = listOf())

    enum class CellType {
        CLOSED,
        FREE
    }

    private fun buildBoard(lines: List<String>): List<List<CellType>> {
        return lines.map { line ->
            line.map { if (it == '#') CellType.CLOSED else CellType.FREE }
        }
    }

    private fun buildPlayers(lines: List<String>, attack: Int = 3): List<Player> {
        val players = mutableListOf<Player>()
        for (y in lines.indices) {
            for (x in lines[y].indices) {
                if (lines[y][x] == 'G') {
                    players.add(Player(x, y, PlayerType.G))
                } else if (lines[y][x] == 'E') {
                    players.add(Player(x, y, PlayerType.E, attackPower = attack))
                }
            }
        }
        return players
    }

    private fun game(players: MutableList<Player>, board: List<List<CellType>>, elfCannotDie: Boolean = false): Int {
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
