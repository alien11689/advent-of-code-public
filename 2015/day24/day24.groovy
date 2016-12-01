weights = new File('day24.data').text.split('\n').collect {it as int}

weightsPerGroup = weights.sum() / 3
println "weightsPerGroup = $weightsPerGroup"

def isValid(buckets){
    buckets.every {it.sum() == weightsPerGroup} && buckets.collectMany{it} as Set == weights as Set
}

long quantum(bucket){
    bucket.inject(1L) {acc, cur -> acc * cur}
}

random = new Random()
best = [
    weights.collect {it},
    [],  
    []
]
bestSize = best[0].size()
bestQuantum = 100000000000l
println best
println bestSize
println bestQuantum

def pack(weights){
    def p = []
    for(w in weights){
        if(((p.sum()?:0) + w) <= weightsPerGroup){
            p << w
        }
    }
    return p
}

memory = [] as Set

def selectSizes(){
    while(true){
        Collections.shuffle(weights)
        def first = pack(weights)
        def second = pack(weights.findAll {!(it in first)})
        def third = pack(weights.findAll {!(it in first)}.findAll {!(it in second)})
//        println "$first.size() $second.size() $third.size()"
//        println "${first.sum()} ${second.sum()} ${third.sum()}"
        def buckets = [first, second, third].sort {it.size()}
        if(isValid(buckets) && !(buckets in memory)){
            memory << buckets
            return buckets
        }
    }
} 

(1..1000000000).each {
    
    def newBuckets = selectSizes()
    println "New Buckets $newBuckets"
        if(newBuckets[0].size() < bestSize){
            best = newBuckets
            bestSize = newBuckets[0].size()
            bestQuantum = quantum(newBuckets[0])
        }else if(newBuckets[0].size() == bestSize && quantum(newBuckets[0]) < bestQuantum){
            best = newBuckets
            bestSize = newBuckets[0].size()
            bestQuantum = quantum(newBuckets[0])
        }

    println "Iteration $it: $best -> ${best[0].size()} -> $bestQuantum"
}
