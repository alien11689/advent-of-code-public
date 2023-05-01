package dpr.aoc2015

object Day19 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getNotEmptyLinesFromFile("/19/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val (molecule, formulas) = readInput(input)
        val ms = mutableSetOf<String>()
        formulas.forEach { f ->
            var i = 0
            while (i < molecule.length) {
                val tail = molecule.drop(i)
                if (tail.startsWith(f.from)) {
                    val begin = molecule.take(i)
                    ms.add(begin + tail.replaceFirst(f.from, f.to))
                }
                ++i
            }
        }
        return ms.size
    }

    private fun readInput(input: List<String>): Pair<String, List<Formula>> {
        var molecule = ""
        val formulas = input.flatMap {
            val parts = it.split(" => ")
            if (parts.size == 2) {
                listOf(Formula(parts[0], parts[1]))
            } else {
                molecule = it
                listOf()
            }
        }
        return Pair(molecule, formulas)
    }

    private fun part2(input: List<String>): Any {
        var (molecule, formulas) = readInput(input)
        var cur = molecule
        formulas = formulas.shuffled()
        var replaceCount = 0
        var prev = cur
        while (cur != "e") {
            formulas.forEach { f ->
                while (cur.contains(f.to)) {
                    cur = cur.replaceFirst(f.to, f.from)
                    ++replaceCount
                }
            }
            if (prev == cur) {
                cur = molecule
                formulas = formulas.shuffled()
                replaceCount = 0
            }
            prev = cur
        }
        return replaceCount
    }

    data class Formula(val from: String, val to: String)
}
