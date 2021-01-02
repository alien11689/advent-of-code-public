//Symulowane wyżarzanie przeważnie nie działa
return

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
        int step = random.nextInt() % 3 == 1 ? 1000 : 1
        [
                new Point(x + step, y, z),
                new Point(x - step, y, z),
                new Point(x, y + step, z),
                new Point(x, y - step, z),
                new Point(x, y, z + step),
                new Point(x, y, z - step),
        ]
    }

    private int changeDist(Random random, int step) {
        (random.nextInt() % 2 == 1 ? step : 0) * (random.nextInt() % 2 == 1 ? -1 : 1)
    }

    int manhattan(Point o) {
        Math.abs(x - o.x) + Math.abs(y - o.y) + Math.abs(z - o.z)
    }
}

int minX = drones.x.min()
int maxX = drones.x.max()
println("x = <${minX},${maxX}> size ${maxX - minX}")
int minY = drones.y.min()
int maxY = drones.y.max()
println("x = <${minY},${maxY}> size ${maxY - minY}")
int minZ = drones.z.min()
int maxZ = drones.z.max()
println("x = <${minZ},${maxZ}> size ${maxZ - minZ}")

int minR = drones.range.min()
int maxR = drones.range.max()
println("x = <${minR},${maxR}> size ${maxR - minR}")

int maxIter = 100000000
int population = 1200
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
    List<Point> newPoints = points.sort { -it.fdest }.subList(0, population / 100 as int).collect {
        it.neighbours(random)[random.nextInt(6)]
    }
    newPoints.addAll(generatePoints(population / 100 * 1100 as int, random, minX, maxX, minY, maxY, minZ, maxZ))
    points = newPoints
    ++iter
}
