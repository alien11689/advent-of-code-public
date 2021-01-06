package pl.touk.dpr.aoc2019

object Day14 {
    @JvmStatic
    fun main(args: Array<String>) {
        val input = Util.getNotEmptyLinesFromFile("/14/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        val initReactions = readReactions(input)
        val fuel = 1L
        return countOre(initReactions, fuel)
    }

    private fun countOre(
        initReactions: List<Reaction>,
        fuel: Long
    ): Long {
        val reactions = initReactions.toMutableList()
        val usedReactions = mutableSetOf<Reaction>()
        val needs = mutableMapOf("FUEL" to fuel).withDefault { 0L }
        var iter = 0
        while (needs.keys != setOf("ORE")) {
            if (reactions.isEmpty()) {
                throw RuntimeException("Empty")
            }
            val basic = reactions.flatMap { it.from }.map { it.name }
            val needed = needs.toList().find { it.first !in basic }
            if (needed == null) {
                throw RuntimeException("Empty needed")
            }
            needs.remove(needed.first)
            val r = reactions.find { it.to.name == needed.first }!!
            reactions.remove(r)
            usedReactions.add(r)
            val times =
                if (needed.second % r.to.amount == 0L) needed.second / r.to.amount else needed.second / r.to.amount + 1
            r.from.forEach { c ->
                needs[c.name] = (needs[c.name] ?: 0L) + times * c.amount
            }
    //            println("${++iter}: Resolved ${needed.first} results $needs")

        }
        return needs["ORE"]!!
    }

    private fun readReactions(input: List<String>) = input.map { line ->
        val (left, right) = line.split("=>")
        val fromRaw = left.split(',')
        Reaction(fromRaw.map { Chemical.create(it) }, Chemical.create(right))
    }

    private fun part2(input: List<String>): Any {
        val initReactions = readReactions(input)
        val expected = 1000000000000L
        var a = 1L
        var b = 10000000L
        var best = 1L
        var bestOre = -1L
        while (a < b) {
            val c = (b + a) / 2
            if (c == a) {
                break
            }
            val ore = countOre(initReactions, c)
//            println("($a,$b): $c => $ore (${ore < expected})")
            if (ore > expected) {
                b = c
            } else {
                a = c
                if(bestOre < ore) {
                    bestOre = ore
                    best = c
                }
            }
        }
//        println("${best} fuel units from $bestOre ore")
        return best
    }

    data class Chemical(val name: String, val amount: Int) {
        companion object {
            fun create(raw: String): Chemical {
                val vals = raw.trim().split(' ')
                if (vals.size == 1) {
                    return Chemical(vals[0], 1)
                } else {
                    return Chemical(vals[1], vals[0].toInt())
                }
            }
        }
    }

    data class Reaction(val from: List<Chemical>, val to: Chemical)
}