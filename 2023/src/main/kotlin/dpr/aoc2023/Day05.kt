package dpr.aoc2023

import dpr.commons.Util

object Day05 {
    private const val INITIAL_STATE = "seed"
    private const val FINAL_STATE = "location"

    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/05/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    data class Translation(val sourceMin: Long, val sourceMax: Long, val dest: Long, val range: Long) {
        fun translate(value: Long): Long? = if (value in sourceMin..sourceMax) {
            dest + (value - sourceMin)
        } else {
            null
        }

        fun translateRange(curSeed: SeedRange): Pair<SeedRange?, List<SeedRange>> {
            when {
                curSeed.max < sourceMin || curSeed.min > sourceMax -> {
                    // ----
                    //      ----
                    // or
                    //      ----
                    // ----
                    return null to listOf(curSeed)
                }

                curSeed.min >= sourceMin && curSeed.max <= sourceMax -> {
                    // ----
                    // ----
                    // or
                    // ---
                    // ----
                    // or
                    //  ---
                    // ----
                    // or
                    //   ---
                    // --------
                    return SeedRange(translate(curSeed.min)!!, translate(curSeed.max)!!) to emptyList()
                }

                curSeed.min <= sourceMin && curSeed.max >= sourceMax -> {
                    // ---
                    // --
                    // or
                    // ---
                    //  --
                    // or
                    // -------
                    //   ---
                    return SeedRange(translate(sourceMin)!!, translate(sourceMax)!!) to listOf(
                        SeedRange(curSeed.min, sourceMin - 1),
                        SeedRange(sourceMax + 1, curSeed.max)
                    ).filter { it.min <= it.max }
                }

                curSeed.min < sourceMin -> {
                    // ---
                    //  ---
                    return SeedRange(translate(sourceMin)!!, translate(curSeed.max)!!) to listOf(
                        SeedRange(
                            curSeed.min,
                            sourceMin - 1
                        )
                    )
                }

                else -> {
                    //  ---
                    // ---
                    return SeedRange(
                        translate(curSeed.min)!!,
                        translate(sourceMax)!!
                    ) to listOf(SeedRange(sourceMax + 1, curSeed.max))
                }
            }
        }
    }

    data class SeedRange(val min: Long, val max: Long) {
        fun translate(transitions: MutableSet<Translation>): List<SeedRange> {
            val newState = mutableListOf<SeedRange>()
            var curSeeds = listOf(this)
            transitions.forEach { transition ->
                val newCurSeeds = mutableListOf<SeedRange>()
                curSeeds.forEach { curSeed ->
                    val (translated, passed) = transition.translateRange(curSeed)
                    translated?.let { newState.add(it) }
                    newCurSeeds.addAll(passed)
                }
                curSeeds = newCurSeeds
            }
            newState.addAll(curSeeds)
            return newState
        }

        val length = max - min + 1
    }

    @JvmStatic
    fun part1(lines: List<String>): Long {
        val seeds = lines[0].split(":")[1].trim().split(" ").map { it.toLong() }
        val mappings = parseMappings(lines)
        var curSeeds = seeds
        var curState = INITIAL_STATE
        while (curState != FINAL_STATE) {
            val (curMapping, transitions) = mappings.filter { it.key.first == curState }.entries.first()
            curState = curMapping.second
            curSeeds =
                curSeeds.map { curValue -> transitions.firstNotNullOfOrNull { it.translate(curValue) } ?: curValue }
        }
        return curSeeds.min()
    }

    @JvmStatic
    fun part2(lines: List<String>): Long {
        val seedsRanges = lines[0].split(":")[1].trim().split(" ").map { it.toLong() }.chunked(2)
            .map { SeedRange(it[0], it[0] + it[1] - 1) }
        val mappings = parseMappings(lines)
        var curSeedRanges = seedsRanges
        var curState = INITIAL_STATE
        while (curState != FINAL_STATE) {
//            println("Cur state is $curState and seedsRanges number is ${curSeedRanges.size} (seeds ${seedsRanges.sumOf { it.length }})")
            val (curMapping, transitions) = mappings.filter { it.key.first == curState }.entries.first()
//                println(" using mapping $transitions")
//                println(" applied transitions ${transitions.map { it.translate(curValue) }}")
            curState = curMapping.second
            curSeedRanges = curSeedRanges.flatMap { it.translate(transitions) }
//            println(" goes to $curState $curSeedRanges")
        }
        return curSeedRanges.minOf { it.min }
    }

    private fun parseMappings(lines: List<String>): MutableMap<Pair<String, String>, MutableSet<Translation>> {
        val mappings = mutableMapOf<Pair<String, String>, MutableSet<Translation>>()
        var currentMapping: Pair<String, String> = "" to ""
        lines.drop(1).forEach { line ->
            if (line.endsWith(":")) {
                val (from, _, to) = line.split(Regex("[ :-]"))
                currentMapping = from to to
                mappings[currentMapping] = mutableSetOf()
            } else {
                val (dest, source, range) = line.split(" ").map { it.toLong() }
                mappings[currentMapping]!!.add(Translation(source, source + range - 1, dest, range))
            }
        }
        return mappings
    }
}
