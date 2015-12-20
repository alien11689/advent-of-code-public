String[] input = new File('day16.data').text.split('\n')

@groovy.transform.ToString
class Aunt{
    String id
    Map<String, Integer> feat
}

aunts = input.collect {it.replaceAll('[:,]', '').split(' ') as List}
    .collect {new Aunt(id: it[1], feat: [
            (it[2]): it[3] as int,
            (it[4]): it[5] as int,
            (it[6]): it[7] as int,
        ])
    }

println aunts

dest = [
    children: 3,
    cats: 7,
    samoyeds: 2,
    pomeranians: 3,
    akitas: 0,
    vizslas: 0,
    goldfish: 5,
    trees: 3,
    cars: 2,
    perfumes: 1
]

println aunts
    .findAll { aunt -> 
        def recipe = dest
            .findAll { it.key in aunt.feat.keySet()}

        def recipeWithoutVarious = recipe.findAll{!(it.key in ['cats', 'trees', 'pomeranians', 'goldfish'])}
        def auntWithoutVarious = aunt.feat.findAll {!(it.key in ['cats', 'trees', 'pomeranians', 'goldfish'])}

        if(recipeWithoutVarious != auntWithoutVarious){
            return false
        }
        def auntsCats = aunt.feat.find {it.key == 'cats'}?.value
        def auntsTrees = aunt.feat.find {it.key == 'trees'}?.value
        def auntsPomeranians = aunt.feat.find {it.key == 'pomeranians'}?.value
        def auntsGoldfish = aunt.feat.find {it.key == 'goldfish'}?.value

        if(auntsCats && !(auntsCats > recipe.cats)){
            return false
        }
        if(auntsTrees && !(auntsTrees > recipe.trees)){
            return false
        }
        if(auntsPomeranians && !(auntsPomeranians < recipe.pomeranians)){
            return false
        }
        if(auntsGoldfish && !(auntsGoldfish < recipe.goldfish)){
            return false
        }
        return true
    }
