package dpr.aoc2015

import dpr.commons.Util

object Day16 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/16/input.txt")
        val sues = readSues(input)
        println(part1(sues))
        println(part2(sues))
    }

    private fun part1(sues: List<Sue>): Any {
        return sues.asSequence()
            .filter { !it.info.containsKey("children") || it.info.getValue("children") == 3 }
            .filter { !it.info.containsKey("cats") || it.info.getValue("cats") == 7 }
            .filter { !it.info.containsKey("samoyeds") || it.info.getValue("samoyeds") == 2 }
            .filter { !it.info.containsKey("pomeranians") || it.info.getValue("pomeranians") == 3 }
            .filter { !it.info.containsKey("akitas") || it.info.getValue("akitas") == 0 }
            .filter { !it.info.containsKey("vizslas") || it.info.getValue("vizslas") == 0 }
            .filter { !it.info.containsKey("goldfish") || it.info.getValue("goldfish") == 5 }
            .filter { !it.info.containsKey("trees") || it.info.getValue("trees") == 3 }
            .filter { !it.info.containsKey("cars") || it.info.getValue("cars") == 2 }
            .filter { !it.info.containsKey("perfumes") || it.info.getValue("perfumes") == 1 }
            .map { it.id }
            .first()
    }

    private fun readSues(input: List<String>) = input.map {
        val parts = it.split(Regex("[ :,]+"))
        val info = mutableMapOf<String, Int>()
        var i = 2
        while (i < parts.size) {
            info[parts[i]] = parts[i + 1].toInt()
            i += 2
        }
        Sue(parts[1].toInt(), info.toMap())
    }

    private fun part2(sues: List<Sue>): Any {
        return sues.asSequence()
            .filter { !it.info.containsKey("children") || it.info.getValue("children") == 3 }
            .filter { !it.info.containsKey("cats") || it.info.getValue("cats") > 7 }
            .filter { !it.info.containsKey("samoyeds") || it.info.getValue("samoyeds") == 2 }
            .filter { !it.info.containsKey("pomeranians") || it.info.getValue("pomeranians") < 3 }
            .filter { !it.info.containsKey("akitas") || it.info.getValue("akitas") == 0 }
            .filter { !it.info.containsKey("vizslas") || it.info.getValue("vizslas") == 0 }
            .filter { !it.info.containsKey("goldfish") || it.info.getValue("goldfish") < 5 }
            .filter { !it.info.containsKey("trees") || it.info.getValue("trees") > 3 }
            .filter { !it.info.containsKey("cars") || it.info.getValue("cars") == 2 }
            .filter { !it.info.containsKey("perfumes") || it.info.getValue("perfumes") == 1 }
            .map { it.id }
            .first()
    }

    data class Sue(val id: Int, val info: Map<String, Int>)

}
