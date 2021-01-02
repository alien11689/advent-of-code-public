import groovy.transform.Canonical

static String readInput(String file) {
    return new File(file).text
}

static List<String> readInputAsLines(String file) {
    return readInput(file).trim().split('\n')
}

List<String> input = readInputAsLines('input.txt')

@Canonical
class Drone {
    int x
    int y
    int z

    int range

    int manhattan(Drone o) {
        Math.abs(x - o.x) + Math.abs(y - o.y) + Math.abs(z - o.z)
    }

    int manhattan(Point o) {
        Math.abs(x - o.x) + Math.abs(y - o.y) + Math.abs(z - o.z)
    }

    Point toPoint() {
        return new Point(x, y, z)
    }

    List<Point> maxNeighbours() {
        List<Point> points = []
        [-1, 0, 1].each { x ->
            [-1, 0, 1].each { y ->
                [-1, 0, 1].each { z ->
                    points << new Point(this.x + x, this.y + y, this.z + z)
                }
            }
        }
        points
    }
}

List<Drone> drones = input.collect { it.split(/[<>,=]+/) }.collect {
    new Drone(it[1] as int, it[2] as int, it[3] as int, it[5] as int)
}

Drone max = drones.max { it.range }

println("Part1")
println(drones.count { it.manhattan(max) <= max.range })

println("Part2")

Point ZERO = new Point(0, 0, 0)

@Canonical
class Point {
    int x
    int y
    int z
    int fdest

    Point(int x, int y, int z) {
        this.x = x
        this.y = y
        this.z = z
    }

    int fDest(List<Drone> drones) {
        int count = 0
        for (int i = 0; i < drones.size(); ++i) {
            count += drones[i].manhattan(this) <= drones[i].range ? 1 : 0
        }
        fdest = count
        return count
    }

    int manhattan(Point o) {
        Math.abs(x - o.x) + Math.abs(y - o.y) + Math.abs(z - o.z)
    }
}

int minX = drones.x.min()
int maxX = drones.x.max()
//println("x = <${minX},${maxX}>")
int minY = drones.y.min()
int maxY = drones.y.max()
//println("x = <${minY},${maxY}>")
int minZ = drones.z.min()
int maxZ = drones.z.max()
//println("x = <${minZ},${maxZ}>")

int minR = drones.range.min()
int maxR = drones.range.max()
//println("x = <${minR},${maxR}>")

int distX = (maxX - minX) / 4
int distY = (maxY - minY) / 4
int distZ = (maxZ - minZ) / 4

Point best = null
int maxFDest = 0
int minDist = 10000000000

int iter = 0
while (distX > 1) {
    ++iter
//    println("Iter $iter - dists ($distX, $distY, $distZ)")
    for (int x = minX; x <= maxX; x += distX) {
        for (int y = minY; y <= maxY; y += distY) {
            for (int z = minZ; z <= maxZ; z += distZ) {
                Point p = new Point(x, y, z)
                int fdest = p.fDest(drones)
                int dist = p.manhattan(ZERO)
                if (fdest > maxFDest || fdest == maxFDest && dist < minDist) {
                    maxFDest = fdest
                    best = p
                    minDist = dist
//                    println("New best $best, count $maxFDest, distTo0 $minDist")
                }
            }
        }
    }
    minX = best.x - distX
    maxX = best.x + distX
    minY = best.y - distY
    maxY = best.y + distY
    minZ = best.z - distZ
    maxZ = best.z + distZ
    distX /= 2
    distY /= 2
    distZ /= 2
}
println(minDist)
