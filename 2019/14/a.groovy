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

Set<Reaction> usedReactions = []

Map<String, Integer> needs = [FUEL: 1].withDefault { 0 }
int iter = 0
while (needs.keySet() != ['ORE']) {
    if (reactions.empty) {
        break
    }
    Set<String> basic = reactions.collectMany { it.from }.collect { it.name }
    Map.Entry<String, Integer> needed = needs.find { !(it.key in basic) }
    if (needed == null) {
        break
    }
    needs.remove(needed.key)
    Reaction r = reactions.find { it.to.name == needed.key }
    reactions.remove(r)
    usedReactions << r
    int amount = 0
    Map<String, Integer> m = [:].withDefault { 0 }
    while (amount < needed.value) {
        r.from.each { c ->
            needs[c.name] += c.amount
        }
        amount += r.to.amount
    }
    println("${++iter}: $needs")
}


