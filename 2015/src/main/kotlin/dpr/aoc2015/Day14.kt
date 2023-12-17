package dpr.aoc2015

import dpr.commons.Util

object Day14 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/14/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        var reeinders: List<Pair<Reeinder, ReeinderState>> = readReeinders(input)
        var t = 0
        while (t < 2503) {
            reeinders = reeinders.map { Pair(it.first, it.first.tick(it.second)) }
            ++t
        }
        return reeinders.maxOf { it.second.dist }
    }

    private fun part2(input: List<String>): Any {
        var reeinders: List<Pair<Reeinder, ReeinderState>> = readReeinders(input)
        var t = 0
        while (t < 2503) {
            reeinders = reeinders.map { Pair(it.first, it.first.tick(it.second)) }
            val leaderDist = reeinders.maxOf { it.second.dist }
            reeinders = reeinders.map {
                if (it.second.dist == leaderDist) {
                    it.copy(second = it.second.copy(score = it.second.score + 1))
                } else {
                    it
                }
            }
            ++t
        }
        return reeinders.maxOf { it.second.score }
    }

    private fun readReeinders(input: List<String>): List<Pair<Reeinder, ReeinderState>> {
        return input.map {
            val parts = it.split(" ")
            Pair(Reeinder(parts[3].toInt(), parts[6].toInt(), parts[13].toInt()), ReeinderState())
        }
    }

    data class ReeinderState(
        val score: Int = 0,
        val isRunning: Boolean = true,
        val resting: Int = 0,
        val running: Int = 0,
        val dist: Int = 0
    )

    data class Reeinder(val speed: Int, val run: Int, val rest: Int) {
        fun tick(state: ReeinderState): ReeinderState {
            if (state.isRunning) {
                val dist = state.dist + speed
                val running = state.running + 1
                return if (running == run) {
                    state.copy(dist = dist, running = 0, isRunning = false)
                } else {
                    state.copy(dist = dist, running = running)
                }
            } else {
                val resting = state.resting + 1
                return if (resting == rest) {
                    state.copy(resting = 0, isRunning = true)
                } else {
                    state.copy(resting = resting)
                }
            }
        }
    }
}
