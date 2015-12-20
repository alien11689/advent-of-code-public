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
    def pre = set.values().findAll {it < 0}.size() == 0 && set.values().sum() == 100
    int calories = set.collect { ingredients[it.key].calories * it.value}.sum()
    pre && calories == 500
}

best = ingredients.keySet().collectEntries {[(it):25]}
bestScore = score(best)

(0..100).each {sp->
    (0..(100 - sp)).each {p->
        (0..(100 - sp - p)).each {f->
            (0..(100 - sp - p - f)).each {su->
                def cur = [
                    Sprinkles: sp,
                    PeanutButter: p,
                    Frosting: f,
                    Sugar: su
                ]
                if(isValid(cur)){
                    println cur
                    def curScore = score(cur)
                    if(curScore > bestScore){
                        bestScore = curScore
                        best = cur
                    }
                }
            }
        }
    }    
}

println best
println bestScore
