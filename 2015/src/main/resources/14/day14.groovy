String [] input = new File('day14.data').text.split('\n')

@groovy.transform.ToString
class Reindeer {
    String name
    int speed
    int fly
    int rest
}

reindeers = input.collect {
    String[] p = it.split(' ')
    new Reindeer(name: p[0], speed: p[3] as int, fly: p[6] as int, rest: p[13] as int)
}

println reindeers

def calculateDist(Reindeer r){
    int time = 2503
    boolean flying = true
    int distance = 0
    int curFly = 0
    int curRest = 0
    while(time > 0){
        if(flying){
            distance += r.speed
            curFly += 1
        }else{
            curRest += 1
        }
        if(curFly == r.fly){
            flying = false
            curFly = 0
        }
        if(curRest == r.rest){
            flying = true
            curRest = 0
        }
        --time
    }
    println "$time -> $distance"
    return distance
}

println reindeers.collect {calculateDist(it)}.max()
