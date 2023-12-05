package dpr.aoc2023

object Day05 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/05/input.txt")
//        val lines = Util.getNotEmptyLinesFromFile("/05/test1.txt")
        println(part1(lines))
        println(part2(lines))
    }

    data class Translation(val sourceMin: Long, val sourceMax: Long, val dest: Long, val range: Long) {
        fun translate(value: Long): Long? = if (value in sourceMin..sourceMax) {
            dest + (value - sourceMin)
        } else {
            null
        }

        fun translateRange(curSeed: SeedRange): Pair<List<SeedRange>, List<SeedRange>> {
            if (curSeed.max < sourceMin || curSeed.min > sourceMax) {
                // ----
                //      ----
                // or
                //
                // ----
                return emptyList<SeedRange>() to listOf(curSeed)
            }
            if (curSeed.min == sourceMin && curSeed.max == sourceMax) {
                // ----
                // ----
                return listOf(SeedRange(translate(curSeed.min)!!, translate(curSeed.max)!!)) to emptyList()
            }
            if (curSeed.min == sourceMin && curSeed.max < sourceMax) {
                // ---
                // ----
                return listOf(SeedRange(translate(curSeed.min)!!, translate(curSeed.max)!!)) to emptyList()
            }
            if (curSeed.min == sourceMin && curSeed.max > sourceMax) {
                // ---
                // --
                return listOf(SeedRange(translate(curSeed.min)!!, translate(sourceMax)!!)) to listOf(SeedRange(sourceMax + 1, curSeed.max))
            }
            if (curSeed.min > sourceMin && curSeed.max == sourceMax) {
                //  --
                // ---
                return listOf(SeedRange(translate(curSeed.min)!!, translate(sourceMax)!!)) to emptyList()
            }
            if (curSeed.min < sourceMin && curSeed.max == sourceMax) {
                // ---
                //  --
                return listOf(SeedRange(translate(sourceMin)!!, translate(sourceMax)!!)) to listOf(SeedRange(curSeed.min, sourceMin - 1))
            }
            if (curSeed.min < sourceMin && curSeed.max > sourceMax) {
                // -------
                //   ---
                return listOf(SeedRange(translate(sourceMin)!!, translate(sourceMax)!!)) to listOf(
                    SeedRange(curSeed.min, sourceMin - 1),
                    SeedRange(sourceMax + 1, curSeed.max)
                )
            }
            if (curSeed.min > sourceMin && curSeed.max < sourceMax) {
                //   ---
                // --------
                return listOf(SeedRange(translate(curSeed.min)!!, translate(curSeed.max)!!)) to emptyList()
            }
            if (curSeed.min < sourceMin && curSeed.max < sourceMax) {
                // ---
                //  ---
                return listOf(SeedRange(translate(sourceMin)!!, translate(curSeed.max)!!)) to listOf(SeedRange(curSeed.min, sourceMin - 1))
            }
            if (curSeed.min > sourceMin && curSeed.max > sourceMax) {
                //  ---
                // ---
                return listOf(SeedRange(translate(curSeed.min)!!, translate(sourceMax)!!)) to listOf(
                    SeedRange(sourceMax + 1, curSeed.max)
                )
            }
            TODO("Not yet implemented")
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
                    newState.addAll(translated)
                    newCurSeeds.addAll(passed)
                }
                curSeeds = newCurSeeds
            }
            newState.addAll(curSeeds)
            return newState
        }

        val length = max - min + 1
    }

    private fun part1(lines: List<String>): Any {
        val seeds = lines[0].split(":")[1].trim().split(" ").map { it.toLong() }
        val mappings = parseMappings(lines)
        var minimal = Long.MAX_VALUE
        for (s in seeds) {
            var curValue = s
            var curState = "seed"
//            println("Seed $curValue")
            while (curState != "location") {
                val (curMapping, transitions) = mappings.filter { it.key.first == curState }.entries.first()
//                println(" using mapping $transitions")
//                println(" applied transitions ${transitions.map { it.translate(curValue) }}")
                curState = curMapping.second
                curValue = transitions.firstNotNullOfOrNull { it.translate(curValue) } ?: curValue
//                println(" goes to $curState $curValue")
            }
//            println("Seed $s -> Location $curValue")
            if (curValue < minimal) {
                minimal = curValue
            }
        }
        return minimal
    }

    private fun parseMappings(lines: List<String>): MutableMap<Pair<String, String>, MutableSet<Translation>> {
        var i = 1
        val mappings = mutableMapOf<Pair<String, String>, MutableSet<Translation>>()
        var currentMapping: Pair<String, String> = "" to ""
        while (i < lines.size) {
            if (lines[i].endsWith(":")) {
                val (from, _, to) = lines[i].split(Regex("[ :-]"))
                currentMapping = from to to
                mappings[currentMapping] = mutableSetOf()
            } else {
                val (dest, source, range) = lines[i].split(" ").map { it.toLong() }
                mappings[currentMapping]!!.add(Translation(source, source + range - 1, dest, range))
            }
            ++i
        }
        return mappings
    }

    private fun part2(lines: List<String>): Any {
        val seedsRanges = lines[0].split(":")[1].trim().split(" ").map { it.toLong() }.chunked(2).map { SeedRange(it[0], it[0] + it[1] - 1) }
        val mappings = parseMappings(lines)
        var curSeedRanges = seedsRanges
        var curState = "seed"
        while (curState != "location") {
            println("Cur state is $curState and seedsRanges number is ${curSeedRanges.size} (seeds ${seedsRanges.sumOf { it.length }})")
            val (curMapping, transitions) = mappings.filter { it.key.first == curState }.entries.first()
//                println(" using mapping $transitions")
//                println(" applied transitions ${transitions.map { it.translate(curValue) }}")
            curState = curMapping.second
            curSeedRanges = curSeedRanges.flatMap { it.translate(transitions) }
//            println(" goes to $curState $curSeedRanges")
        }
        return curSeedRanges.minOf { it.min }
    }
}

