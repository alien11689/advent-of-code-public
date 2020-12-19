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

println aunts.findAll { aunt -> aunt.feat == dest.findAll { it.key in aunt.feat.keySet()} }
