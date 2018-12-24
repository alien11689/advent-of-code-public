import groovy.transform.Canonical

enum Team {
    Immune,
    Infection
}

@Canonical
class Group implements Comparable<Group> {
    Team team
    int id
    int units
    int hitPoints
    List<String> weak
    List<String> immune
    int initiative
    int attack
    String attackType
    Group target = null
    Integer attacker = null

    int effectivePower() {
        return units * attack
    }

    @Override
    int compareTo(Group o) {
        def powerA = effectivePower()
        def powerB = o.effectivePower()
        if (powerA != powerB) {
            return -powerA <=> -powerB
        }
        return -initiative <=> -o.initiative
    }

    void reset() {
        target = null
        attacker = null
    }

    void attack() {
        if (target == null || units <= 0) {
            return
        }
        target.takeDamage(target.weak.contains(attackType) ? effectivePower() * 2 : effectivePower())
    }

    void takeDamage(int damage) {
        println("\tI take $damage damage and I have $hitPoints ,")
        int toKill = damage / hitPoints as int
        println("\tTo kill $toKill")
        units -= toKill
    }
}

String bludgeoning = 'bludgeoning'
String cold = 'cold'
String slashing = 'slashing'
String fire = 'fire'
String radiation = 'radiation'
List<Group> groups = [
//        new Group(Team.Immune, 1, 17, 5390, ['radiation', 'bludgeoning'], [], 2, 4507, 'fire'),
//        new Group(Team.Immune, 2, 989, 1274, ['bludgeoning', 'slashing'], ['fire'], 3, 25, 'slashing'),
//        new Group(Team.Infection, 1, 801, 4706, ['radiation'], [], 1, 116, 'bludgeoning'),
//        new Group(Team.Infection, 2, 4485, 2961, ['fire', 'cold'], ['radiation'], 4, 12, 'slashing')

new Group(Team.Immune, 1, 1432, 7061, [bludgeoning], [cold], 17, 41, slashing),
new Group(Team.Immune, 2, 3387, 9488, [bludgeoning], [], 20, 27, slashing),
new Group(Team.Immune, 3, 254, 3249, [], [fire], 1, 89, cold),
new Group(Team.Immune, 4, 1950, 8201, [], [], 15, 39, fire),
new Group(Team.Immune, 5, 8137, 3973, [slashing], [radiation], 6, 4, radiation),
new Group(Team.Immune, 6, 4519, 7585, [fire], [], 8, 15, radiation),
new Group(Team.Immune, 7, 763, 7834, [fire], [radiation, slashing, cold], 18, 91, radiation),
new Group(Team.Immune, 8, 935, 10231, [], [slashing, cold], 12, 103, bludgeoning),
new Group(Team.Immune, 9, 4557, 7860, [], [slashing], 11, 15, slashing),
new Group(Team.Immune, 10, 510, 7363, [fire, radiation], [], 5, 143, fire),

new Group(Team.Infection, 1, 290, 29776, [cold, radiation], [], 16, 204, bludgeoning),
new Group(Team.Infection, 2, 7268, 14114, [bludgeoning], [radiation], 19, 3, bludgeoning),
new Group(Team.Infection, 3, 801, 5393, [], [], 13, 13, slashing),
new Group(Team.Infection, 4, 700, 12182, [], [], 4, 29, cold),
new Group(Team.Infection, 5, 531, 16607, [], [slashing], 10, 53, bludgeoning),
new Group(Team.Infection, 6, 23, 24482, [cold, fire], [], 7, 2095, bludgeoning),
new Group(Team.Infection, 7, 8025, 43789, [cold], [radiation], 9, 8, radiation),
new Group(Team.Infection, 8, 1405, 53896, [], [], 14, 70, slashing),
new Group(Team.Infection, 9, 566, 7820, [], [cold], 2, 26, cold),
new Group(Team.Infection, 10, 1641, 7807, [fire], [slashing, bludgeoning], 3, 7, radiation),
]

while ((groups.team as Set).size() > 1) {
    println('-------------------')
    groups.sort { [it.team, it.id] }.each {
        println("${it.team} ${it.id} has ${it.units}")
    }
    groups = groups.sort()
    groups.each { it.reset() }
    // selection
    groups.each { cur ->
        Integer target = null
        int mostDamage = -1
        int opponentEffectivePower = -1
        int initiative = -1
        groups.findAll { it.team != cur.team }.each { opponent ->
            if (opponent.attacker != null) {
                return
            }
            int damage
            if (cur.attackType in opponent.immune) {
                damage = 0
            } else {
                damage = cur.effectivePower()
                if (opponent.weak.contains(cur.attackType)) {
                    damage *= 2
                }
            }

            if (damage == 0) {
                return
            }
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
        if (target) {
            Group targetGroup = groups.find { it.team != cur.team && it.id == target }
            cur.target = targetGroup
            targetGroup.attacker = cur.id
        }
    }
//    groups.each {
//        println("${it.team} ${it.id} chooses opponent ${it.target?.id}")
//    }
    // attack
    groups.sort { -it.initiative }.each {
        println("${it.team} ${it.id} attacks ${it.target?.id}")
        it.attack()
    }
    // remove dead
    groups.removeAll { it.units <= 0 }
}

println("Winner ${groups[0].team}")
println(groups.sum { it.units })