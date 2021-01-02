import groovy.transform.Immutable

List<String> lines = new File('input.txt').text.split('\n')

@Immutable
class Point {
    int x
    int y

    Set<Point> neighbours() {
        return [
                new Point(x + 1, y),
                new Point(x - 1, y),
                new Point(x, y + 1),
                new Point(x, y - 1),
        ] as Set
    }
}

Map<Point, Boolean> map = [:]

Map<Set<Point>, Set<String>> warps = [:]

int sizeY = lines.size()
int sizeX = lines[2].size()

//println("Maze is $sizeX x $sizeY")

for (int j = 0; j < sizeY; j++) {
    for (int i = 0; i < lines[j].size(); i++) {
        Point p = new Point(i, j)
        String s = lines[j][i]
        switch (s) {
            case '.': map[p] = true; break
            case '#': map[p] = false; break
            case ' ': break
            default:
//                map[p] = true;
                Map<Point, String> m = (p.neighbours() + p).findAll {
                    it.x >= 0 && it.x < lines[j].size() && it.y >= 0 && it.y < sizeY
                }.collectEntries { [(it): lines[it.y][it.x]] }.findAll { !(it.value in [' ', '#', '.']) }
                warps[m.keySet()] = m.values() as Set
                break
        }
    }
}

Map<Point, Set<String>> betterWarps = warps.collectEntries {
    Point k = it.key.collectMany { it.neighbours() }.find { map[it] }
    return [(k): it.value]
}

Set<Point> warpPoints = betterWarps.keySet()


Point start = betterWarps.find { it.value == ['A'] as Set }.key
//println(start)
Point dest = betterWarps.find { it.value == ['Z'] as Set }.key
//println(dest)

@Immutable
class PathElement {
    Set<String> warp
    Point from
    Point to
    int level
    int length
}

@Immutable
class State implements Comparable<State> {
    Point cur
    int length
    List<PathElement> path = []
    int level = 0

    @Override
    int compareTo(State o) {
        return length <=> o.length
    }
}

PriorityQueue<State> pq = new PriorityQueue<>()
pq.offer(new State(start, 0, [], 0))

@Immutable
class Visited {
    Point p
    int level
}

Set<Point> outerWarps = betterWarps.keySet().findAll { it.x in [2, sizeX - 3] || it.y in [2, sizeY - 3] }
Set<Point> innerWarps = betterWarps.keySet() - outerWarps

Set<Point> visited = [] as Set

while (!pq.empty) {
//    println("pq size ${pq.size()}")
    State state = pq.poll()
//    println("Checking $state")
//    println("Length ${state.length}")
    if (state.cur in dest && state.level == 0) {
        println("State $state")
        println("Get in ${state.length} steps")
        break
    }

    if (new Visited(state.cur, state.level) in visited) {
        continue
    }
    visited << new Visited(state.cur, state.level)

    state.cur.neighbours().findAll { map[it] }.each { Point checkedPoint ->
        if (checkedPoint in [dest, start] && state.level != 0) {
            // dest and start are walls on inner levels
        } else if (checkedPoint in warpPoints && state.level == 0 && !(checkedPoint in [dest, start]) && checkedPoint in outerWarps) {
            // on level 0 outers do not work
        } else {
            if (checkedPoint in warpPoints && !(checkedPoint in [dest, start])) {
                Set<String> warp = betterWarps[checkedPoint]
//                println("Using warp $warp")
                Point to = betterWarps.find { it.value == warp && it.key != checkedPoint }.key
                int level
                if (checkedPoint in outerWarps) {
                    level = state.level - 1
                } else {
                    level = state.level + 1
                }
                PathElement element = new PathElement(warp, checkedPoint, to, level, state.length + 2)
                if (!state.path.empty && state.path[-1].warp == warp) {
                    // do nothing - cycle
                } else {
                    pq.offer(new State(to, state.length + 2, state.path + element, level))
                }
            } else {
                pq.offer(new State(checkedPoint, state.length + 1, state.path, state.level))
            }
        }
    }

}
