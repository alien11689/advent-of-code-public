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

Set<Reaction> usedReactions = []

Map<String, Long> needs = [FUEL: 1L].withDefault { 0L }
int iter = 0
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
    
    println("${++iter}: Resolved ${needed.key} results $needs")
}


