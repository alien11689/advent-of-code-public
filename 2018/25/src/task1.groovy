import groovy.transform.Canonical

static String readInput(String file) {
    return new File(file).text
}

static List<String> readInputAsLines(String file) {
    return readInput(file).trim().split('\n').collect { it.trim() }
}

List<String> lines = readInputAsLines('input.txt')
lines = readInputAsLines('other.txt')

@Canonical
class Point {
    int w
    int x
    int y
    int z

    int manhattan(Point o) {
        Math.abs(w - o.w) + Math.abs(x - o.x) + Math.abs(y - o.y) + Math.abs(z - o.z)
    }
}

List<Point> readPoints(List<String> lines) {
    lines.collect { line ->
        int[] parts = line.split(/[ ,]+/).collect { it as int }
        new Point(w: parts[0], x: parts[1], y: parts[2], z: parts[3])
    }
}

//println(readPoints(lines))


Set<Set<Point>> findConstellation(List<Point> points) {
    Set<Set<Point>> constellations = [] as Set
    while (!points.empty) {
        println("Constellations ${constellations.size()}")
        Set<Point> constel = [] as Set
        Stack<Point> neighbours = new Stack<Point>()
        Point root = points.pop()
        neighbours.push(root)
        constel << root
        while (!neighbours.empty) {
            println("Neighbours ${neighbours.size()}; points ${points.size()}")
            Point next = neighbours.pop()
            Set<Point> nextNeighbours = points.findAll { it.manhattan(next) <= 3 }
            neighbours.addAll(nextNeighbours)
            constel.addAll(nextNeighbours)
            points.removeAll(nextNeighbours)
        }
        println("Found constellation with size ${constel.size()}")
        constellations << constel
    }
    return constellations
}

println(findConstellation(readPoints(readInputAsLines('other.txt'))).size())
println(findConstellation(readPoints(readInputAsLines('other2.txt'))).size())
println(findConstellation(readPoints(readInputAsLines('other3.txt'))).size())
println(findConstellation(readPoints(readInputAsLines('other4.txt'))).size())
println(findConstellation(readPoints(readInputAsLines('input.txt'))).size())