import groovy.transform.Immutable

List<String> lines = new File('input.txt').text.split('\n')

@Immutable
class Chemical {
    String name
    int amount

    static Chemical create(String raw) {
        String[] vals = raw.trim().split(' ')
        if (vals.length == 1) {
            return new Chemical(vals[0], 1)
        } else {
            return new Chemical(vals[1], vals[0] as int)
        }
    }
}

@Immutable
class Reaction {
    List<Chemical> from
    Chemical to
}

Set<Reaction> reactions = lines.collect { line ->
    def (left, right) = line.split("=>")
    List<String> fromRaw = left.split(',')
    new Reaction(fromRaw.collect { Chemical.create(it) }, Chemical.create(right))
}

//reactions.each {
//    println(it)
//}

long countOre(Set<Reaction> reactions, long fuel) {
    Set<Reaction> usedReactions = []

    Map<String, Long> needs = [FUEL: fuel].withDefault { 0L }
    while (needs.keySet() != ['ORE']) {
        if (reactions.empty) {
            break
        }
        Set<String> basic = reactions.collectMany { it.from }.collect { it.name }
        Map.Entry<String, Long> needed = needs.find { !(it.key in basic) }
        if (needed == null) {
            break
        }
        needs.remove(needed.key)
        Reaction r = reactions.find { it.to.name == needed.key }
        reactions.remove(r)
        usedReactions << r
        long times = needed.value % r.to.amount == 0 ? needed.value / r.to.amount : ((long) (needed.value / r.to.amount) + 1)
        r.from.each { c ->
            needs[c.name] += times * c.amount
        }
        //println("${++iter}: $needs")
    }

    return needs['ORE']
}

long expected = 1000000000000L
long a = 1L
long b = 10000000L
long bestOre = -1L
while (a < b) {
    long c = (b + a) / 2
    long ore = countOre(new HashSet<>(reactions), c)
    println("($a,$b): $c => $ore (${ore < expected})")
    if (c == a) {
        break
    }
    if (ore > expected) {
        b = c
    } else {
        a = c
        bestOre = ore
    }
}
println("${a} fuel units from $bestOre ore")
