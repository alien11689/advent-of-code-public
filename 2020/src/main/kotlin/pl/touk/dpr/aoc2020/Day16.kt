package pl.touk.dpr.aoc2020

object Day16 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        val input = Util.getLinesFromFile("/16/input.txt")
        println(part1(input))
        println(part2(input))
    }

    private fun part1(input: List<String>): Any {
        var i = 0
        val rules = mutableSetOf<Rule>()
        var readingRules = true
        var nearbyTickets = false
        var sumNotMatching = 0L
        while (i < input.size) {
            if (input[i].isEmpty()) {
                readingRules = false
                nearbyTickets = false
                ++i
                continue
            } else if (readingRules) {
                val parts = input[i].split(":")[1].split(Regex("[ -]+"))
                rules.add(Rule(IntRange(parts[1].toInt(), parts[2].toInt()), IntRange(parts[4].toInt(), parts[5].toInt())))
                ++i
            } else if (input[i] == "nearby tickets:") {
                nearbyTickets = true
                ++i
            } else if (nearbyTickets) {
                val parts = input[i].split(',').asSequence().map { it.toInt() }
                parts.filter { v -> !rules.any { it.match(v) } }.forEach { sumNotMatching += it }
                ++i
            } else {
                ++i
            }

        }
        return sumNotMatching
    }

    private fun part2(input: List<String>): Any {
        var i = 0
        val rules = mutableSetOf<Rule>()
        var readingRules = true
        var nearbyTickets = false
        var myTicktet = false
        var myTicktetValue = listOf(0)
        val otherValidTickets = mutableSetOf<List<Int>>()
        while (i < input.size) {
            if (input[i].isEmpty()) {
                readingRules = false
                nearbyTickets = false
                myTicktet = false
                ++i
                continue
            } else if (readingRules) {
                val split1 = input[i].split(":")
                val parts = split1[1].split(Regex("[ -]+"))
                rules.add(Rule(IntRange(parts[1].toInt(), parts[2].toInt()), IntRange(parts[4].toInt(), parts[5].toInt()), split1[0]))
                ++i
            } else if (input[i] == "your ticket:") {
                myTicktet = true
                ++i
            } else if (myTicktet) {
                myTicktetValue = input[i].split(",").map { it.toInt() }
                ++i
            } else if (input[i] == "nearby tickets:") {
                nearbyTickets = true
                ++i
            } else if (nearbyTickets) {
                val parts = input[i].split(',').map { it.toInt() }
                if (parts.all { v -> rules.any { it.match(v) } }) {
                    otherValidTickets.add(parts.toList())
                }
                ++i
            } else {
                ++i
            }
        }
        i = 0
        val columntToRules = mutableMapOf<Int, Set<Rule>>()
        while (i < otherValidTickets.first().size) {
            val column = otherValidTickets.map { it[i] }
            val matchingRules = rules.filter { r -> column.all { r.match(it) } }
            columntToRules[i] = matchingRules.toSet()
            ++i
        }
        val departureColumns = mutableSetOf<Int>()
        while (columntToRules.isNotEmpty()) {
            val found = columntToRules.filter { it.value.size == 1 }
            found.forEach { (k, v) ->
                columntToRules.remove(k)
                columntToRules.forEach { (col, rules) ->
                    columntToRules[col] = rules - v
                }
                if (v.count { it.name!!.startsWith("departure") } > 0) {
                    departureColumns.add(k)
                }
            }
        }

        return departureColumns.fold(1L) { acc, col -> acc * myTicktetValue[col] }
    }

    data class Rule(val r1: IntRange, val r2: IntRange, val name: String? = null) {
        fun match(v: Int) = r1.contains(v) || r2.contains(v)
    }

    // Column 12 match [Rule(r1=47..536, r2=557..964, name=class)]
    //Column 9 match [Rule(r1=40..90, r2=104..961, name=arrival location), ]
    //Column 7 match [, Rule(r1=45..416, r2=427..959, name=arrival track), ]
    //Column 11 match [, , , Rule(r1=28..726, r2=748..954, name=train)]
    //Column 10 match [, , , Rule(r1=33..229, r2=246..969, name=duration), ]
    //Column 4 match [, , , , Rule(r1=49..292, r2=307..964, name=seat), ]
    //Column 3 match [, , , , Rule(r1=32..328, r2=349..970, name=route), , ]
    //Column 16 match [, , , , , , , Rule(r1=37..430, r2=438..950, name=type)]
    //Column 6 match [Rule(r1=32..600, r2=611..967, name=departure platform), , , , , , , , ]
    //Column 1 match [, Rule(r1=50..766, r2=776..972, name=departure time), , , , , , , , ]
    // Column 13 match [, Rule(r1=44..452, r2=473..965, name=departure track), , , , , , , , , ]
    //Column 2 match [Rule(r1=29..856, r2=863..974, name=departure station), , , , , , , , , , , ]
    //Column 14 match [, , , Rule(r1=36..115, r2=129..950, name=departure date), , , , , , , , , ]
    //Column 15 match [Rule(r1=30..260, r2=284..950, name=departure location), , , , , , , , , , , , , ]
    //Column 18 match [, , , , , , , Rule(r1=40..864, r2=887..971, name=arrival station), , , , , , , ]
    //Column 17 match [, , , , , , , , , , , , Rule(r1=50..692, r2=709..964, name=row), , , ]
    //Column 5 match [, , , , , , , , Rule(r1=32..920, r2=932..964, name=arrival platform), , , , , , , , ]
    //Column 0 match [, , , , , , , , , , , , , , , , , Rule(r1=46..628, r2=638..973, name=wagon)]
    //Column 8 match [, , , , , , , , , , , , , , , , , , Rule(r1=39..786, r2=807..969, name=zone)]
    //Column 19 match [, , , , , , , , , , , , Rule(r1=25..147, r2=172..969, name=price), , , , , , , ]
}

