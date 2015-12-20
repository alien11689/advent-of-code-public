String [] input = new File('day15.data').text.split('\n')

int maxTeaspoons = 100

@groovy.transform.ToString
class Ingredient {
    String name
    int capacity
    int durability
    int flavor
    int texture
    int calories
}

ingredients = input.collectEntries {
    String[] p = it.replaceAll(/[:,]/,'').split(' ')
    [(p[0]): new Ingredient(
        name: p[0], 
        capacity: p[2] as int, 
        durability: p[4] as int,
        flavor: p[6] as int,
        texture: p[8] as int,
        calories: p[10] as int,
    )]
}

println ingredients

def score(Map<String, Integer> set){
    int capacity = set.collect { ingredients[it.key].capacity * it.value}.sum()
    int durability = set.collect { ingredients[it.key].durability * it.value}.sum()
    int flavor = set.collect { ingredients[it.key].flavor * it.value}.sum()
    int texture = set.collect { ingredients[it.key].texture * it.value}.sum()

    def list = [capacity, durability, flavor, texture]
    
    list.findAll{it < 0}.size() > 0 ? 0 : list.inject(1) {a, b -> a * b}
}

def isValid(Map<String, Integer> set){
    set.values().findAll {it < 0}.size() == 0 && set.values().sum() == 100
}

best = ingredients.keySet().collectEntries {[(it):25]}
println isValid(best)
bestScore = score(best)

def memory = [] as Set
def random = new Random()

def ings = ingredients.keySet() as List

(1..10000).each {
    String from = ings[random.nextInt() % ingredients.size()]
    String to = ings[random.nextInt() % ingredients.size()]
    int change = random.nextInt() % 3 + 1

    def newSet = best.collectEntries {it}
    newSet[from] += change
    newSet[to] -= change

    if(isValid(newSet)){
        newScore = score(newSet)
        if(newScore > bestScore){
            best = newSet
            bestScore = newScore
        }
    }

    println "$it: $best => $bestScore"
}
