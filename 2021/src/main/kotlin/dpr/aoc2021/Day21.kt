package dpr.aoc2021

import dpr.commons.Util
import java.util.Stack

object Day21 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        // positions are decremented for easier modulo calculations
        println(part1())
        println(part2())
    }

    @JvmStatic
    fun part1(
        player1: Player = Player(1, 2, 0),
        player2: Player = Player(2, 6, 0)
    ): Long {
        val deterministicDice = DeterministicDie()
        while (player1.score < 1000 || player2.score < 1000) {
            val rolls1 = listOf(deterministicDice.roll(), deterministicDice.roll(), deterministicDice.roll()).sum()
            player1.move(rolls1)
            if (player1.score >= 1000) {
                return 1L * player2.score * deterministicDice.rolls
            }

            val rolls2 = listOf(deterministicDice.roll(), deterministicDice.roll(), deterministicDice.roll()).sum()
            player2.move(rolls2)
            if (player2.score >= 1000) {
                // println(deterministicDice)
                // println(player1)
                // println(player2)
                return 1L * player1.score * deterministicDice.rolls
            }
        }
        return -1
    }

    data class DeterministicDie(var cur: Int = 1, var rolls: Int = 0) {
        fun roll(): Int {
            val c = cur
            ++cur
            if (cur == 101) {
                cur = 1
            }
            ++rolls
            return c
        }
    }

    data class Player(val id: Int, var pos: Int, var score: Int) {
        fun move(rolls1: Int) {
            pos = (pos + rolls1) % 10
            score += pos + 1
        }
    }

    @JvmStatic
    fun part2(
        player1: PlayerV2 = PlayerV2(1, 2, 0),
        player2: PlayerV2 = PlayerV2(2, 6, 0)
    ): Long {
        val wins = mutableMapOf(1 to 0L, 2 to 0L)
        val finalScore = 21
        val stack = Stack<Pair<PlayerV2, PlayerV2>>()
        stack.push(Pair(player1, player2))
        val rollsToUniverses = mapOf(
            3 to 1,
            4 to 3,
            5 to 6,
            6 to 7,
            7 to 6,
            8 to 3,
            9 to 1
        )
        while (!stack.isEmpty()) {
            val (p1, p2) = stack.pop()
            for (rolls in rollsToUniverses) {
                val newP1 = p1.addRolls(rolls.key, rolls.value)
                if (newP1.score >= finalScore) {
                    wins[newP1.id] = wins[newP1.id]!! + newP1.universes * p2.universes
                } else {
                    stack.push(p2 to newP1)
                }
            }
        }

        //157134537817 is wrong
        return wins.values.maxOrNull()!!
    }

    data class PlayerV2(val id: Int, val pos: Int, val score: Int, val universes: Long = 1) {
        fun addRolls(rolls: Int, inUniverses: Int): PlayerV2 {
            val newPos = (pos + rolls) % 10
            val newScore = score + newPos + 1
            val newUniverses = universes * inUniverses
            return copy(pos = newPos, score = newScore, universes = newUniverses)
        }
    }
}


