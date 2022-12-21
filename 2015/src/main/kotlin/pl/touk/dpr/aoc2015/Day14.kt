package pl.touk.dpr.aoc2015

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
        return reeinders.map { it.second.dist }.maxOrNull()!!
    }

    private fun part2(input: List<String>): Any {
        var reeinders: List<Pair<Reeinder, ReeinderState>> = readReeinders(input)
        var t = 0
        while (t < 2503) {
            reeinders = reeinders.map { Pair(it.first, it.first.tick(it.second)) }
            val leaderDist = reeinders.map { it.second.dist }.maxOrNull()!!
            reeinders = reeinders.map {
                if (it.second.dist == leaderDist) {
                    it.copy(second = it.second.copy(score = it.second.score + 1))
                } else {
                    it
                }
            }
            ++t
        }
        return reeinders.map { it.second.score }.maxOrNull()!!
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
                if (running == run) {
                    return state.copy(dist = dist, running = 0, isRunning = false)
                } else {
                    return state.copy(dist = dist, running = running)
                }
            } else {
                val resting = state.resting + 1
                if (resting == rest) {
                    return state.copy(resting = 0, isRunning = true)
                } else {
                    return state.copy(resting = resting)
                }
            }
        }
    }
}
