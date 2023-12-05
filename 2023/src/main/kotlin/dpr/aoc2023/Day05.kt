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
        fun translate(value: Long): Long? {
            if (value in sourceMin..sourceMax) {
                return dest + (value - sourceMin)
            } else {
                return null
            }
        }
    }

    private fun part1(lines: List<String>): Any {
        var i = 0
        val seeds = lines[i].split(":")[1].trim().split(" ").map { it.toLong() }
        println(seeds)
        ++i
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
        var minimal = Long.MAX_VALUE
        for (s in seeds) {
            var curValue = s
            var curState = "seed"
            println("Seed $curValue")
            while (curState != "location") {
                val (curMapping, transitions) = mappings.filter { it.key.first == curState }.entries.first()
//                println(" using mapping $transitions")
//                println(" applied transitions ${transitions.map { it.translate(curValue) }}")
                curState = curMapping.second
                curValue = transitions.firstNotNullOfOrNull { it.translate(curValue) } ?: curValue
                println(" goes to $curState $curValue")
            }
            println("Seed $s -> Location $curValue")
            if (curValue < minimal) {
                minimal = curValue
            }
        }
        return minimal
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

