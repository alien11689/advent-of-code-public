import groovy.transform.EqualsAndHashCode
import groovy.transform.Immutable

List<List<Character>> input = new File('input.txt').text.trim().split('\n')

//println(new LocalState(new Point(41,41), 0) == new LocalState(new Point(41,41), 7))
//return

@Immutable
@EqualsAndHashCode
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

    List<Point> diag() {
        [
                new Point(x + 1, y + 1),
                new Point(x - 1, y + 1),
                new Point(x + 1, y - 1),
                new Point(x - 1, y - 1),
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

List<Point> curPoses = curPos.diag()
curPos.neighbours().each {
    map[it] = false
}
//
//for (int j = 0; j < input.size(); j++) {
//    for (int i = 0; i < input[j].size(); i++) {
//        char field = input[j][i]
//        Point point = new Point(i, j)
//        print("${map[point] ? '.' : '#'}")
//    }
//    println()
//}


@Immutable
class State implements Comparable<State> {
    List<LocalState> localStates
    Map<Point, Character> toVisit = [:]
    List<Character> path = []

    boolean ended() {
        toVisit.isEmpty()
    }

    int compareTo(State o) {
        int lengthCompare = localStates.sum { it.length } <=> o.localStates.sum { it.length }
        if (lengthCompare) {
            return toVisit.size() <=> o.toVisit.size()
        }
        return lengthCompare
    }
}

@Immutable
@EqualsAndHashCode
class LocalState implements Comparable<LocalState> {
    Point cur
    int length

    int compareTo(LocalState o) {
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

State state = new State(curPoses.collect { new LocalState(it, 0) }, keysAndDoors, [])

PriorityQueue<State> pq = new PriorityQueue<>()
pq.offer(state)

Set mem = [] as Set

while (!pq.empty) {
    State s = pq.poll()
//    println("=========================")
    if (s.toVisit in mem) {
        continue
    }
    mem << s.toVisit
//    println("Checking $s")
//    println(s.localStates.sum { it.length })
    if (s.ended()) {
        println("Ended $s")
        println(s.localStates.sum { it.length })
        break
    }
    s.localStates.each { LocalState ls ->
//        println("Checking local state $ls")
        Map<Character, Integer> reachable = findReachable(ls.cur, map, s.toVisit)
        reachable.each { Map.Entry<Character, Integer> e ->
            char key = e.key
            int moves = e.value
            Point nextPos = keysAndDoors.find { it.value == key }.key
            Map<Point, Character> newKeysAndDoors = s.toVisit.findAll { it.value != key && it.value != key.toUpperCase() }
            LocalState newLocalState = new LocalState(nextPos, ls.length + moves)
//            println("current local states " + s.localStates)
            List<LocalState> newLocalStates = s.localStates.findAll { local ->  !local.equals(ls) } + newLocalState
//            println("new local states " + newLocalStates)
            State newState = new State(newLocalStates, newKeysAndDoors, s.path + key)
            pq.offer(newState)
        }
    }
}

//122 is too low

