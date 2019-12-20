import groovy.transform.Immutable

List<String> lines = new File('input.txt').text.split('\n')
//lines = new File('input1.txt').text.split('\n')

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

for (int j = 0; j < lines.size(); j++) {
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
                    it.x >= 0 && it.x < lines[j].size() && it.y >= 0 && it.y < lines.size()
                }.collectEntries { [(it): lines[it.y][it.x]] }.findAll { !(it.value in [' ', '#', '.']) }
                warps[m.keySet()] = m.values() as Set
                break
        }
    }
}

//println(warps)


Map<Point, Set<String>> betterWarps = warps.collectEntries {
    Point k = it.key.collectMany { it.neighbours() }.find { map[it] }
//    Point k = it.key.find { it.neighbours().findAll { map[it] }.size() > 1 }
//    it.key.find { it.neighbours().findAll { map[it] }.size() == 1 }.each { map[it] = false }
    return [(k): it.value]
}

Set<Point> warpPoints = betterWarps.keySet()

//println(warps.size())
println(betterWarps.size())

println(betterWarps)

Point start = betterWarps.find { it.value == ['A'] as Set }.key
println(start)
Point dest = betterWarps.find { it.value == ['Z'] as Set }.key
println(dest)

Set<Point> visited = [] as Set

@Immutable
class State implements Comparable<State> {
    Point cur
    int length
    List<Set<String>> path = []

    @Override
    int compareTo(State o) {
        return length <=> o.length
    }
}

PriorityQueue<State> pq = new PriorityQueue<>()
pq.offer(new State(start, 0, []))

while (!pq.empty) {
    println("pq size ${pq.size()}")
    State state = pq.poll()
    println("Checking $state")
    if (state.cur in dest) {
        println("Get in ${state.length} steps")
        break
    }
    if (state.cur in visited) {
        continue
    }
    visited << state.cur
    state.cur.neighbours().findAll { map[it] }.each { Point checkedPoint ->
        if (checkedPoint in warpPoints && !(checkedPoint in [dest, start])) {
            Set<String> warp = betterWarps[checkedPoint]
            println("Using warp $warp")
            Point to = betterWarps.find { it.value == warp && it.key != checkedPoint }.key
            pq.offer(new State(to, state.length + 2, state.path + [warp]))
        } else {
            pq.offer(new State(checkedPoint, state.length + 1, state.path))
        }
    }

}

// 666 is wrong
// 644 is wrong