package pl.touk.dpr.aoc2020

import java.util.LinkedList
import java.util.Queue

object Day22 {
    @JvmStatic
    fun main(args: Array<String>) {
//        val input = Util.getNotEmptyLinesFromFile("/22/sample.txt")
        val input = Util.getNotEmptyLinesFromFile("/22/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val (player1: Queue<Int>, player2: Queue<Int>) = readPlayers(input)
        while (player1.isNotEmpty() && player2.isNotEmpty()) {
            val p1 = player1.poll()
            val p2 = player2.poll()
            if (p1 > p2) {
                player1.offer(p1)
                player1.offer(p2)
            } else {
                player2.offer(p2)
                player2.offer(p1)
            }
        }
        val winner = if (player1.isEmpty()) player2 else player1
        return winner.reversed().foldIndexed(0L) { index, acc, i -> acc + (index + 1) * i.toLong() }
    }

    private fun readPlayers(input: List<String>): Pair<Queue<Int>, Queue<Int>> {
        val player1: Queue<Int> = LinkedList()
        val player2: Queue<Int> = LinkedList()
        var reading1 = true
        input.forEach {
            if (it.startsWith("Player")) {
                if (it.endsWith("2:")) {
                    reading1 = false
                }
            } else {
                if (reading1) {
                    player1.offer(it.toInt())
                } else {
                    player2.offer(it.toInt())
                }
            }
        }
        return Pair(player1, player2)
    }

    private fun part2(input: List<String>): Any {
        val (player1: Queue<Int>, player2: Queue<Int>) = readPlayers(input)
        val (p1, p2) = play(player1, player2)
        val winner = if (p1.isEmpty()) p2 else p1
        return winner.reversed().foldIndexed(0L) { index, acc, i -> acc + (index + 1) * i.toLong() }
    }

    private fun play(initPlayer1: Queue<Int>, initPlayer2: Queue<Int>, level: Int = 1): Pair<Queue<Int>, Queue<Int>> {
        println("Current level is $level with $initPlayer1 $initPlayer2")
        val player1 = LinkedList(initPlayer1)
        val player2 = LinkedList(initPlayer2)
        val prev = mutableSetOf<Pair<List<Int>, List<Int>>>()
        while (player1.isNotEmpty() && player2.isNotEmpty()) {
            val curState = Pair(player1.toList(), player2.toList())
            if (prev.contains(curState)) {
                while (player2.isNotEmpty()) {
                    player1.offer(player2.poll())
                }
                return Pair(player1, player2)
            }
            prev.add(curState)
            val p1 = player1.poll()
            val p2 = player2.poll()
            if (p1 <= player1.size && p2 <= player2.size) {
                println("Starting subgame because of $p1 $p2")
                val (pp1, pp2) = play(player1, player2, level + 1)
                if (pp1.isNotEmpty()) {
                    player1.offer(p1)
                    player1.offer(p2)
                } else {
                    player2.offer(p2)
                    player2.offer(p1)
                }
                println("Return to level $level with $player1 $player2")
            } else {
                if (p1 > p2) {
                    player1.offer(p1)
                    player1.offer(p2)
                } else {
                    player2.offer(p2)
                    player2.offer(p1)
                }
            }
        }
        return Pair(player1, player2)
    }
}
