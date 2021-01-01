package pl.touk.dpr.aoc2018

object Day09 {
    @JvmStatic
    fun main(args: Array<String>) {
        println(part1())
        println(part2())
    }

    private fun part1(): Any {
        val playersCount = 465
        val maxScore = 71498
        return findHighestScore(playersCount, maxScore)
    }

    private fun part2(): Any {
        val playersCount = 465
        val maxScore = 71498
        return findHighestScore(playersCount, maxScore * 100)
    }

    data class Player(val id: Int, var score: Long = 0L) {
        fun addScore(score: Number) {
            this.score += score.toLong()
        }
    }

    data class Node(val value: Int, var prev: Node? = null, var next: Node? = null) {

        fun insertAfter(score: Int): Node {
            val newNode = Node(score)
            newNode.prev = this.next
            newNode.next = this.next!!.next
            this.next!!.next!!.prev = newNode
            this.next!!.next = newNode
            return newNode
        }

        fun removeBehind(): Pair<Node, Int> {
            var toRemove = this
            var count = 0
            while (count < 7) {
                count++
                toRemove = toRemove.prev!!
            }
            val value = toRemove.value
            val newCur = toRemove.next!!
            toRemove.prev!!.next = toRemove.next
            toRemove.next!!.prev = toRemove.prev
            return Pair(newCur, value)
        }
    }

    fun findHighestScore(playersCount: Int, maxScore: Int): Long {
        val players = (1..playersCount).map { Player(it) }

        val root = Node(0)
        root.prev = root
        root.next = root

        var score = 1
        var curPlayer = 0

        var cur = root

        while (score <= maxScore) {
            if (score % 23 == 0) {
                players[curPlayer].addScore(score)
                val result = cur.removeBehind()
                players[curPlayer].addScore(result.second)
                cur = result.first
            } else {
                cur = cur.insertAfter(score)
            }
            score++
            curPlayer = (curPlayer + 1) % playersCount
        }
        return players.maxByOrNull { it.score }!!.score
    }
}