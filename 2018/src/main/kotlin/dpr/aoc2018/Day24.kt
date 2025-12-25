package dpr.aoc2018

import dpr.commons.Util

object Day24 {
    @JvmStatic
    fun main(args: Array<String>) = Util.measureTime {
        println(part1())
        println(part2())
    }

    @JvmStatic
    fun part1(): Int {
        val groups = generateGroups()
        val res = fight(groups)
        return res.sum!!
    }

    @JvmStatic
    fun part2(): Int {
        var minBound = 0
        var maxBound = 1000
        var score = 0
        while (maxBound >= minBound) {

            val groups = generateGroups()

            val boost = minBound + (maxBound - minBound) / 2
//    println("Checking boost $boost")
            groups.filter { it.team == Team.Immune }.forEach { it.attack += boost }

            val result = fight(groups)
            if (result.team == Team.Immune) {
                score = result.sum!!
                maxBound = boost - 1
            } else {
                minBound = boost + 1
            }

        }
        return score
    }

    enum class Team {
        Immune,
        Infection
    }

    data class Group(
        val team: Team,
        val id: Int,
        var units: Int,
        val hitPoints: Int,
        val weak: List<String>,
        val immune: List<String>,
        val initiative: Int,
        var attack: Int,
        val attackType: String,
        var target: Group? = null,
        var attacker: Int? = null
    ) : Comparable<Group> {
        fun effectivePower() = units * attack

        override fun compareTo(other: Group): Int {
            val powerA = effectivePower()
            val powerB = other.effectivePower()
            if (powerA != powerB) {
                return (-powerA).compareTo(-powerB)
            }
            return (-initiative).compareTo(-other.initiative)
        }

        fun reset() {
            target = null
            attacker = null
        }

        fun attack(): Int {
            if (target == null || units <= 0) {
                return 0
            }
            return target!!.takeDamage(if (target!!.weak.contains(attackType)) effectivePower() * 2 else effectivePower())
        }

        private fun takeDamage(damage: Int): Int {
//        println("\tI take $damage damage and I have $hitPoints ,")
            val toKill = damage / hitPoints
//        println("\tTo kill $toKill")
            units -= toKill
            return toKill
        }
    }

    private fun generateGroups(): MutableList<Group> {
        val bludgeoning = "bludgeoning"
        val cold = "cold"
        val slashing = "slashing"
        val fire = "fire"
        val radiation = "radiation"

        return mutableListOf(

            Group(Team.Immune, 1, 1432, 7061, listOf(bludgeoning), listOf(cold), 17, 41, slashing),
            Group(Team.Immune, 2, 3387, 9488, listOf(bludgeoning), listOf(), 20, 27, slashing),
            Group(Team.Immune, 3, 254, 3249, listOf(), listOf(fire), 1, 89, cold),
            Group(Team.Immune, 4, 1950, 8201, listOf(), listOf(), 15, 39, fire),
            Group(Team.Immune, 5, 8137, 3973, listOf(slashing), listOf(radiation), 6, 4, radiation),
            Group(Team.Immune, 6, 4519, 7585, listOf(fire), listOf(), 8, 15, radiation),
            Group(Team.Immune, 7, 763, 7834, listOf(fire), listOf(radiation, slashing, cold), 18, 91, radiation),
            Group(Team.Immune, 8, 935, 10231, listOf(), listOf(slashing, cold), 12, 103, bludgeoning),
            Group(Team.Immune, 9, 4557, 7860, listOf(), listOf(slashing), 11, 15, slashing),
            Group(Team.Immune, 10, 510, 7363, listOf(fire, radiation), listOf(), 5, 143, fire),

            Group(Team.Infection, 1, 290, 29776, listOf(cold, radiation), listOf(), 16, 204, bludgeoning),
            Group(Team.Infection, 2, 7268, 14114, listOf(bludgeoning), listOf(radiation), 19, 3, bludgeoning),
            Group(Team.Infection, 3, 801, 5393, listOf(), listOf(), 13, 13, slashing),
            Group(Team.Infection, 4, 700, 12182, listOf(), listOf(), 4, 29, cold),
            Group(Team.Infection, 5, 531, 16607, listOf(), listOf(slashing), 10, 53, bludgeoning),
            Group(Team.Infection, 6, 23, 24482, listOf(cold, fire), listOf(), 7, 2095, bludgeoning),
            Group(Team.Infection, 7, 8025, 43789, listOf(cold), listOf(radiation), 9, 8, radiation),
            Group(Team.Infection, 8, 1405, 53896, listOf(), listOf(), 14, 70, slashing),
            Group(Team.Infection, 9, 566, 7820, listOf(), listOf(cold), 2, 26, cold),
            Group(Team.Infection, 10, 1641, 7807, listOf(fire), listOf(slashing, bludgeoning), 3, 7, radiation),
        )
    }

    data class Result(val team: Team? = null, val sum: Int? = null, val groups: List<Group> = listOf())

    private fun fight(initGroups: MutableList<Group>): Result {
        while (initGroups.map { it.team }.toSet().size > 1) {
//            println(initGroups)
            val groups = initGroups.sorted()
            groups.forEach { it.reset() }
            // selection
            groups.forEach { cur ->
                var target: Int? = null
                var mostDamage = -1
                var opponentEffectivePower = -1
                var initiative = -1
                groups.filter { it.team != cur.team }.forEach { opponent ->
                    if (opponent.attacker == null) {
                        var damage: Int
                        if (cur.attackType in opponent.immune) {
                            damage = 0
                        } else {
                            damage = cur.effectivePower()
                            if (opponent.weak.contains(cur.attackType)) {
                                damage *= 2
                            }
                        }

                        if (damage != 0) {
                            if (damage > mostDamage) {
                                target = opponent.id
                                mostDamage = damage
                                opponentEffectivePower = opponent.effectivePower()
                                initiative = opponent.initiative
                            } else if (damage == mostDamage && opponent.effectivePower() > opponentEffectivePower) {
                                target = opponent.id
                                opponentEffectivePower = opponent.effectivePower()
                                initiative = opponent.initiative
                            } else if (damage == mostDamage && opponent.effectivePower() == opponentEffectivePower && opponent.initiative > initiative) {
                                target = opponent.id
                                initiative = opponent.initiative
                            }
                        }
                    }
                }
                if (target != null) {
                    val targetGroup = groups.find { it.team != cur.team && it.id == target }!!
                    cur.target = targetGroup
                    targetGroup.attacker = cur.id
                }
            }
            // attack
            val killed = groups.sortedBy { -it.initiative }.sumOf {
//            println("${it.team} ${it.id} attacks ${it.target?.id}")
                it.attack()
            }
            if (killed == 0) {
//            println('Infinite loop')
                return Result(groups = groups)
            }
            // remove dead
            initGroups.removeAll { it.units <= 0 }
        }
//    println("Winner ${groups[0].team}")
        return Result(initGroups[0].team, initGroups.sumOf { it.units }, initGroups)
    }
}
