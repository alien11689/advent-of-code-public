package dpr.aoc2023

object Day08 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val lines = Util.getNotEmptyLinesFromFile("/08/input.txt")
        println(part1(lines))
        println(part2(lines))
    }

    private fun part1(lines: List<String>): Any {
        val directions = lines[0]
        val mapping = mutableMapOf<String, Pair<String, String>>()
        lines.drop(1).forEach {
            val split = it.split(Regex("[ = (,)]+"))
            mapping[split[0]] = split[1] to split[2]
        }
        var cur = "AAA"
        var steps = 0
        while (cur != "ZZZ") {
            val curMapping = mapping[cur]!!
            cur = if (directions[steps % directions.length] == 'L') curMapping.first else curMapping.second
            ++steps
        }
        return steps
    }

    private fun part2(lines: List<String>): Any {
        TODO()
    }
}

