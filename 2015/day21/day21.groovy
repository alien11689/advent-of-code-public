class Boss {
    int hitPoints = 104
    int damage = 8
    int armor = 1
}

WEAPON = 1
ARMOR = 2
RING = 3

@groovy.transform.ToString
class Item {
    String name
    int type
    int cost
    int damage
    int armor
}

items = [
    new Item(name: 'Dagger', type: WEAPON, cost: 8, damage: 4, armor: 0),
    new Item(name: 'Shortsword', type: WEAPON, cost: 10, damage: 5, armor: 0),
    new Item(name: 'Warhammer', type: WEAPON, cost: 25, damage: 6, armor: 0),
    new Item(name: 'Longsword', type: WEAPON, cost: 40, damage: 7, armor: 0),
    new Item(name: 'Greataxe', type: WEAPON, cost: 74, damage: 8, armor: 0),

    new Item(name: 'Leather', type: ARMOR, cost: 13, damage: 0, armor: 1),
    new Item(name: 'Chainmail', type: ARMOR, cost: 31, damage: 0, armor: 2),
    new Item(name: 'Splintmail', type: ARMOR, cost: 53, damage: 0, armor: 3),
    new Item(name: 'Bandedmail', type: ARMOR, cost: 75, damage: 0, armor: 4),
    new Item(name: 'Platemail', type: ARMOR, cost: 102, damage: 0, armor: 5),

    new Item(name: 'Damage +1', type: RING, cost: 25, damage: 1, armor: 0),
    new Item(name: 'Damage +2', type: RING, cost: 50, damage: 2, armor: 0),
    new Item(name: 'Damage +3', type: RING, cost: 100, damage: 3, armor: 0),
    new Item(name: 'Defense +1', type: RING, cost: 20, damage: 0, armor: 1),
    new Item(name: 'Defense +2', type: RING, cost: 40, damage: 0, armor: 2),
    new Item(name: 'Defense +3', type: RING, cost: 80, damage: 0, armor: 3),
]

weapons = items.findAll {it.type == WEAPON}
armors = items.findAll {it.type == ARMOR}
rings = items.findAll {it.type == RING}
    
def cost(items){
    items.collect {it.cost}.sum()
}

class Hero {
    List items
    int hitPoints = 100

    def fight(Boss b){
        def cur = this
        def damage = items.collect {it.damage}.sum()
        def armor = items.collect {it.armor}.sum()

        while(true){
            if(cur == this){
                def curDamage = [damage - b.armor, 1].max()
                b.hitPoints -= curDamage
                if(b.hitPoints <= 0){
                    return true
                }
                cur = b
            } else {
                def curDamage = [b.damage - armor, 1].max()
                hitPoints -= curDamage
                if(hitPoints <= 0){
                    return false
                }
                cur = this
            }
        }

    }
}

best = [weapons[-1], armors[-1], rings[2], rings[-1]]
bestCost = cost(best)

random = new Random()

(1..10000).each {
    List newItems = []
    newItems << weapons[random.nextInt() % weapons.size()]
    if(random.nextInt() % 2 == 1){
        newItems << armors[random.nextInt() % armors.size()]
    }
    double r = random.nextDouble()
    int amount = r <= 0.33 ? 0 : (r <= 0.66 ? 1 : 2)
    amount.times{
        newItems << rings[random.nextInt() % rings.size()]
    }
    newItems = newItems.unique()
    
    def hero = new Hero(items: newItems)

    if(hero.fight(new Boss())){
        int c = cost(newItems)
        if(c < bestCost){
            best = newItems
            bestCost = c
        }
    }
    println "Iteration $it: $best -> $bestCost"
}

println best
println bestCost
