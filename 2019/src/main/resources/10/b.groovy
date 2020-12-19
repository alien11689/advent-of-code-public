import groovy.transform.Immutable

List<String> lines = new File('input.txt').text.trim().split('\n')

@Immutable
class Point {
    int x
    int y
}

Set<Point> points = [] as Set

for (int i = 0; i < lines.size(); ++i) {
    for (int j = 0; j < lines[i].size(); ++j) {
        if (lines[i][j] == '#') {
            points << new Point(j, i)
        }
    }
}

enum Ord {
    X_UP, RIGHT_UP, Y_RIGHT, RIGHT_DOWN, X_DOWN, LEFT_DOWN, Y_LEFT, LEFT_UP
}

double calcA(Point p1, Point p2) {
    if (p1.x == p2.x) {
        return 10000000
    } else {
        return (p2.y - p1.y) / (p2.x - p1.x)
    }
}

Ord getOrd(Point p1, Point p2) {
    if (p1.x == p2.x) {
        return p1.y < p2.y ? Ord.X_DOWN : Ord.X_UP
    }
    if (p1.y == p2.y) {
        return p1.x < p2.x ? Ord.Y_RIGHT : Ord.Y_LEFT
    }
    if (p1.x < p2.x) {
        return p1.y < p2.y ? Ord.RIGHT_DOWN : Ord.RIGHT_UP
    }
    return p1.y < p2.y ? Ord.LEFT_DOWN : Ord.LEFT_UP
}

def manhattan(Point p1, Point p2) {
    (p1.x - p2.x).abs() + (p1.y - p2.y).abs()
}

Map<Point, Integer> p2size = points.collectEntries { Point center ->
    int size = (points.findAll { it != center }.collect { Point other ->
        [calcA(center, other), getOrd(center, other)]
    } as Set).size()
    [(center): size]
}
def res1 = p2size.max { it.value }
Point center = res1.key

///////////////////////////////////

@Immutable
class Stats implements Comparable<Stats> {
    Ord ord
    double a

    @Override
    int compareTo(Stats o) {
        if (ord == o.ord) {
            switch (ord) {
                case Ord.LEFT_UP: return a.compareTo(o.a)
                case Ord.LEFT_DOWN: return a.compareTo(o.a)
                case Ord.RIGHT_DOWN: return a.compareTo(o.a)
                case Ord.RIGHT_UP: return a.compareTo(o.a)
            }
        }
        return ord.ordinal().compareTo(o.ord.ordinal())
    }
}

Map p2stats = points.findAll { it != center }.collectEntries { p ->
    [(p): new Stats(getOrd(center, p), calcA(center, p))]
}
Map stats2p = [:]
p2stats.each {
    if (it.value in stats2p) {
        stats2p[it.value] << it.key
    } else {
        stats2p[it.value] = [it.key]
    }
}

stats2p.values().each { List<Point> l ->
    l.sort { manhattan(center, it) }
}

List<Stats> stats = stats2p.keySet().sort()
int i = 0

while (i < 200) {
    List ps = stats2p[stats[i % stats.size()]]
    Point p = ps.removeAt(0)
    stats2p[stats[i % stats.size()]] = ps
    if (++i == 200) {
        println(p.x * 100 + p.y)
        break
    }
}

//stats2p.keySet().sort().each {
//    ++i
//    if (i == 200) {
//        println(stats2p[it])
//    }
//    println(++i + ": " + it + " -> " + stats2p[it])
//}
