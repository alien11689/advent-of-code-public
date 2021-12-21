package pl.touk.dpr.aoc2021

object Day21 {
    @JvmStatic
    fun main(args: Array<String>) {
        val lines = emptyList<String>()// Util.getNotEmptyLinesFromFile("/21/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        // test
//        val player1 = Player(1, 3, 0) // one less pos
//        val player2 = Player(2, 7, 0) // one less pos
        // my
        val player1 = Player(1, 2, 0) // one less pos
        val player2 = Player(2, 6, 0) // one less pos

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

    private fun part2(lines: List<String>): Any {
// test
//        val player1 = Player(1, 3, 0) // one less pos
//        val player2 = Player(2, 7, 0) // one less pos
        // my
        val player1 = Player(1, 2, 0) // one less pos
        val player2 = Player(2, 6, 0) // one less pos

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
                return 1L * player1.score * deterministicDice.rolls
            }
        }
        return -1
    }
}


