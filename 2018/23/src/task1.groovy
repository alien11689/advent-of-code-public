import groovy.transform.Canonical

static String readInput(String file) {
    return new File(file).text
}

static List<String> readInputAsLines(String file) {
    return readInput(file).trim().split('\n')
}

List<String> input = readInputAsLines('input.txt')
//input = readInputAsLines('other.txt')
//input = readInputAsLines('other2.txt')

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
}

List<Drone> drones = input.collect { it.split(/[<>,=]+/) }.collect {
    new Drone(it[1] as int, it[2] as int, it[3] as int, it[5] as int)
}

Drone max = drones.max { it.range }

println("Part1")
println(drones.count { it.manhattan(max) <= max.range })

println("Part2")

@Canonical
class Point {
    int x
    int y
    int z
    int fdest

    int fDest(List<Drone> drones) {
        int count = drones.count { it.manhattan(this) <= it.range }
        fdest = count
        return count
    }

    List<Point> neighbours(Random random) {
        int step = random.nextInt() % 10 + 1
        int dx = random.nextInt() % 2 == 1 ? step : 0
        int dy = random.nextInt() % 2 == 1 ? step : 0
        int dz = random.nextInt() % 2 == 1 ? step : 0
        [
                new Point(x + dx, y + dy, z + dz),
                new Point(x - dx, y - dy, z - dz),
                new Point(x + dx, y - dy, z + dz),
                new Point(x - dx, y + dy, z - dz),
                new Point(x + dx, y + dy, z - dz),
                new Point(x - dx, y - dy, z + dz),
        ]
    }

    int manhattan(Point o) {
        Math.abs(x - o.x) + Math.abs(y - o.y) + Math.abs(z - o.z)
    }
}

int minX = drones.x.min()
int maxX = drones.x.max()
println("x = <${minX},${maxX}>")
int minY = drones.y.min()
int maxY = drones.y.max()
println("x = <${minY},${maxY}>")
int minZ = drones.z.min()
int maxZ = drones.z.max()
println("x = <${minZ},${maxZ}>")

int minR = drones.range.min()
int maxR = drones.range.max()
println("x = <${minR},${maxR}>")

//println("minR + 50%: ${int percent = (maxR - minR) / 5*4; drones.count { it.range <= minR + percent } / drones.size()}")

int maxIter = 1000
int population = 6000
Random random = new Random(System.currentTimeMillis())

def nextInt(Random r, int min, int max) {
    return r.nextInt(max + 1 - min) + min
}

List<Point> points = generatePoints(population, random, minX, maxX, minY, maxY, minZ, maxZ)

private List<Point> generatePoints(int population, Random random, int minX, int maxX, int minY, int maxY, int minZ, int maxZ) {
    (1..population).collect {
        new Point(
                x: nextInt(random, minX, maxX),
                y: nextInt(random, minY, maxY),
                z: nextInt(random, minZ, maxZ)
        )
    }
}

points[0] = new Point(23214783, 52699925, 53755415)
points[1] = new Point(24566547, 51779123, 52088929)
points[2] = new Point(24566496, 51779078, 52088906)
points[3] = new Point(24566474, 51779064, 52088892)

int iter = 0
Point best = new Point(0, 0, 0)
int bestCount = 0
int bestDist = 1000000000
while (iter < maxIter) {
    println("Iter $iter; best ${best}; bestCount $bestCount; distTo0: $bestDist")
    for (int i = 0; i < points.size(); ++i) {
        Point it = points[i]
//        println("Checking point $it")
        int cel = it.fDest(drones)
        if (cel > bestCount) {
            println("Changing best to $it")
            best = it
            bestCount = cel
            bestDist = it.manhattan(new Point(0, 0, 0))
        } else if (cel == bestCount) {
            int distTo0 = it.manhattan(new Point(0, 0, 0))
            if (bestDist > distTo0) {
                best = it
                bestDist = distTo0
            }
        }
    }
    if (iter % 5 == 0) {
        List<Point> newPoints = points.subList(0, population / 10 as int)
        newPoints.addAll(generatePoints(population / 10 * 9 as int, random, minX, maxX, minY, maxY, minZ, maxZ))
    } else {
        points = points.sort { -it.fdest }.subList(0, population / 6 as int).collectMany { it.neighbours(random) }
    }
    ++iter
}
