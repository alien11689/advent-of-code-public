String [] input = new File('day14.data').text.split('\n')

@groovy.transform.ToString
class Reindeer {
    String name
    int maxSpeed
    int fly
    int rest
    boolean flying = true
    int distance = 0
    int curFly = 0
    int curRest = 0
    int score = 0

    def tick(){
        if(flying){
            distance += maxSpeed
            ++curFly
        }else{
            ++curRest
        }
        if(curFly == fly){
            flying = false
            curFly = 0
        }
        if(curRest == rest){
            flying = true
            curRest = 0
        }
        return distance
    }
}

reindeers = input.collect {
    String[] p = it.split(' ')
    new Reindeer(name: p[0], maxSpeed: p[3] as int, fly: p[6] as int, rest: p[13] as int)
}

println reindeers

(1..2503).each {
    println "Iteration $it"
    int max = reindeers.collect { it.tick()}.max()
    reindeers.findAll {it.distance == max}.each{ ++it.score }
}

println reindeers
println reindeers.max {it.score}
