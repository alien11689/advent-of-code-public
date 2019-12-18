import groovy.transform.Immutable

List<List<Character>> input = new File('input.txt').text.trim().split('\n')

@Immutable
class Point {
    int x
    int y

    List<Point> neighbours() {
        [
                new Point(x + 1, y),
                new Point(x - 1, y),
                new Point(x, y + 1),
                new Point(x, y - 1),
        ]
    }
}

Map<Point, Boolean> map = [:]
Map<Point, Character> keysAndDoors = [:]

Point curPos = null
for (int j = 0; j < input.size(); j++) {
    for (int i = 0; i < input[j].size(); i++) {
        char field = input[j][i]
        Point point = new Point(i, j)
        switch (field) {
            case '#': map[point] = false; break
            case '.': map[point] = true; break
            case '@': map[point] = true; curPos = point; break
            default: map[point] = true; keysAndDoors[point] = field; break
        }
    }
}

@Immutable
class State implements Comparable<State> {
    Point cur
    Map<Point, Character> toVisit = [:]
    int length = 0
    List<Character> path = []

    boolean ended() {
        toVisit.isEmpty()
    }

    int compareTo(State o) {
        if (length == o.length) {
            return toVisit.size() <=> o.toVisit.size()
        }
        return length <=> o.length
    }
}

@Immutable
class LocalState implements Comparable<LocalState> {
    Point cur
    int length

    int compareTo(LocalState o){
        return length <=> o.length
    }
}

Map<Character, Integer> findReachable(Point start, Map<Point, Boolean> map, Map<Point, Character> keysAndDoors) {
    Set<Point> visited = [] as Set
    PriorityQueue pq = new PriorityQueue()
    pq.offer(new LocalState(start, 0))
    Map<Character, Integer> result = [:]
    while (!pq.empty) {
        LocalState localState = pq.poll()
        Point cur = localState.cur
        visited << cur
        cur.neighbours().findAll { it in map && map[it] && !(it in visited) }.each {
            LocalState newLocalState = new LocalState(it, localState.length + 1)
            if (newLocalState.cur in keysAndDoors) {
                Character keyOrDoor = keysAndDoors[newLocalState.cur]
                if (keyOrDoor.toUpperCase() != keyOrDoor) {
                    result[keyOrDoor] = newLocalState.length
                }
            } else {
                pq.offer(newLocalState)
            }
        }
    }
    return result
}

State state = new State(curPos, keysAndDoors, 0, [])

PriorityQueue<State> pq = new PriorityQueue<>()
pq.offer(state)

Set mem = [] as Set

while (!pq.empty) {
    State s = pq.poll()
//    println("=========================")
    if ([s.cur, s.toVisit] in mem) {
        continue
    }
    mem << [s.cur, s.toVisit]
//    println("Checking $s")
    if (s.ended()) {
        println("Ended $s")
        break
    }
    println(s.length)
    Map<Character, Integer> reachable = findReachable(s.cur, map, s.toVisit)
//    println("Reachable " + reachable)
    reachable.each { Map.Entry<Character, Integer> e ->
        char key = e.key
//        println("Key $key")
        int moves = e.value
        Point nextPos = keysAndDoors.find { it.value == key }.key
        Map<Point, Character> newKeysAndDoors = s.toVisit.findAll { it.value != key && it.value != key.toUpperCase() }
        State newState = new State(nextPos, newKeysAndDoors, s.length + moves, s.path + key)
//        println("Adding new state " + newState)
        pq.offer(newState)
    }
}

// 3426 is too high
// 3224 is too high
// 3068 is too high
