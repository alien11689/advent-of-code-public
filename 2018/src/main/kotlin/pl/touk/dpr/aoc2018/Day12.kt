package pl.touk.dpr.aoc2018

object Day12 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/12/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val initialState = input[0].split(":")[1].trim()
        val rules = readRules(input.drop(1))
        var generation = 0
        var state = mutableMapOf<Int, Char>()
        initialState.forEachIndexed { idx, it ->
            state[idx] = it
        }
        while (generation < 20) {
//            println(generation)
//            println(state)
            generation++
            val max = state.keys.max()!!
            val min = state.keys.min()!!
            state[min - 4] = '.'
            state[min - 3] = '.'
            state[min - 2] = '.'
            state[min - 1] = '.'
            state[max + 1] = '.'
            state[max + 2] = '.'
            state[max + 3] = '.'
            state[max + 4] = '.'
            val newGeneration = state.map { it.toPair() }.toMap().toMutableMap()
            val min1 = state.keys.min()!!
            val max1 = state.keys.max()!!
            for (i in min1..max1) {
                val sb = StringBuilder()
                for (j in (i - 2)..(i + 2)) {
                    sb.append(state.getOrDefault(j, '.'))
                }
                val slice = sb.toString()
                val toReplace = rules.mapNotNull { it.tryApply(slice) }.firstOrNull()
                if (toReplace != null) {
                    newGeneration[i] = toReplace
                } else {
                    newGeneration[i] = '.'
                }
            }
            state = newGeneration
        }
        return state.filter { it.value == '#' }.map { it.key }.sum()
    }

    private fun part2(input: List<String>): Any {
        val initialState = input[0].split(":")[1].trim()
        val rules = readRules(input.drop(1))
        var generation = 0
        val maxIter = 50000000000
        var state = mutableMapOf<Int, Char>()
        initialState.forEachIndexed { idx, it ->
            state[idx] = it
        }
        while (true) {
//            println(generation)
//            println(state)
            generation++

            val max = state.keys.max()!!
            val min = state.keys.min()!!
            state[min - 4] = '.'
            state[min - 3] = '.'
            state[min - 2] = '.'
            state[min - 1] = '.'
            state[max + 1] = '.'
            state[max + 2] = '.'
            state[max + 3] = '.'
            state[max + 4] = '.'
            val newGeneration = state.map { it.toPair() }.toMap().toMutableMap()
            val min1 = state.keys.min()!!
            val max1 = state.keys.max()!!
            for (i in min1..max1) {
                val sb = StringBuilder()
                for (j in (i - 2)..(i + 2)) {
                    sb.append(state.getOrDefault(j, '.'))
                }
                val slice = sb.toString()
                val toReplace = rules.mapNotNull { it.tryApply(slice) }.firstOrNull()
                if (toReplace != null) {
                    newGeneration[i] = toReplace
                } else {
                    newGeneration[i] = '.'
                }
            }
            if (stateAsString(state) == stateAsString(newGeneration)) {
//                println(stateAsString(state))
//                println(generation)
                val base = state.filter { it.value == '#' }.keys.sum()
//                println(base)
                val newGenerationValue = newGeneration.filter { it.value == '#' }.keys.sum()
//                println(newGenerationValue)
                val stepAfterBase = newGenerationValue - base
//                println(stepAfterBase)
                return (maxIter - generation + 1) * stepAfterBase + base
            }
            state = newGeneration
        }
    }

    private fun readRules(rawRules: List<String>): List<Rule> {
        return rawRules
            .map {
                val split = it.split(" => ")
                Rule(split[0].trim(), split[1].trim().first())
            }
    }

    data class Rule(val from: String, val to: Char) {
        fun tryApply(slice: String): Char? {
            return if (slice == from) {
                to
            } else null
        }
    }

    private fun stateAsString(state: Map<Int, Char>): String {
        val min = state.filter { it.value == '#' }.minByOrNull { it.key }!!.key
        val max = state.filter { it.value == '#' }.maxByOrNull { it.key }!!.key
        val sb = StringBuilder()
        for (i in min..max + 1) {
            sb.append(state[i])
        }
        return sb.toString()
    }
}