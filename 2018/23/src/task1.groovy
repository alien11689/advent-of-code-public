import groovy.transform.Canonical

static String readInput(String file) {
    return new File(file).text
}

static List<String> readInputAsLines(String file) {
    return readInput(file).trim().split('\n')
}

List<String> input = readInputAsLines('input.txt')
//input = readInputAsLines('other.txt')

@Canonical
class Drone {
    int x
    int y
    int z

    int range

    int manhattan(Drone o) {
        Math.abs(x - o.x) + Math.abs(y - o.y) + Math.abs(z - o.z)
    }
}

List<Drone> drones = input.collect { it.split(/[<>,=]+/) }.collect {
    new Drone(it[1] as int, it[2] as int, it[3] as int, it[5] as int)
}

Drone max = drones.max { it.range }

println(drones.count {it.manhattan(max) <= max.range})
