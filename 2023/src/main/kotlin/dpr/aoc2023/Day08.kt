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
        val mapping = readMapping(lines)
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
        val directions = lines[0]
        val mapping = readMapping(lines)
        val startingPoints = mapping.filter { it.key.endsWith("A") }.map { it.key to 0 }
        val endPoints = mapping.filter { it.key.endsWith("Z") }.keys
        println(startingPoints)
        println(endPoints)
        var steps = 0
        var cur = startingPoints[5].first
        var previousSeen = 0
        while (steps < 100000) {
            if (cur.endsWith("Z")) {
                println("On step $steps (delta = ${steps - previousSeen})")
                previousSeen = steps
            }
            val curMapping = mapping[cur]!!
            cur = if (directions[steps % directions.length] == 'L') curMapping.first else curMapping.second
            ++steps
        }
        return listOf(16343L, 16897L, 20221L, 18559L, 11911L, 21883L).fold(1L) { acc, cur -> nww(acc, cur) }
        // 27011809034838424525673297 is too high
    }

    private fun nww(x: Long, y: Long): Long {
        return (x * y) / nwd(x, y);
    }

    private fun nwd(xx: Long, yy: Long): Long {
        var x = xx
        var y = yy
        while (x != y) {
            if (x > y)
                x -= y;
            else
                y -= x;
        }
        return x;
    }

    private fun readMapping(lines: List<String>): MutableMap<String, Pair<String, String>> {
        val mapping = mutableMapOf<String, Pair<String, String>>()
        lines.drop(1).forEach {
            val split = it.split(Regex("[ = (,)]+"))
            mapping[split[0]] = split[1] to split[2]
        }
        return mapping
    }
}

