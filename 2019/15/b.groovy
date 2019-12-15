import groovy.transform.Canonical

List<Long> input = new File('input.txt').text.split(',').collect { it as long } as ArrayList
Map<Long, Long> v = [:].withDefault { 0L }
for (long i = 0; i < input.size(); ++i) {
    v[i] = input[i]
}

long param(Map<Long, Long> v, long pos, int mode, long rel) {
    if (mode == 0) {
        return v[v[pos]]
    } else if (mode == 1) {
        return v[pos]
    } else {
        return v[rel + v[pos]]
    }
}

int p1Mode(int op) {
    return ((op / 100) as int) % 10
}

int p2Mode(int op) {
    return ((op / 1000) as int) % 10
}

int p3Mode(int op) {
    return ((op / 10000) as int) % 10
}

void assignTo(Map<Long, Long> v, long pos, int mode, long value, long rel) {
    switch (mode) {
        case 0: v[v[pos]] = value; break
        case 1: v[pos] = value; break
        case 2: v[rel + v[pos]] = value; break
    }
}

void program(State s, Queue<Long> output) {
    Map<Long, Long> v = s.v
    long pos = s.pos
    long rel = s.rel

    while (true) {
        int op = v[pos]
        switch (op % 100) {
            case 99:
                s.ended = true
                return
            case 1:
                assignTo(v, pos + 3, p3Mode(op), param(v, pos + 1, p1Mode(op), rel) + param(v, pos + 2, p2Mode(op), rel), rel)
                pos += 4
                break
            case 2:
                assignTo(v, pos + 3, p3Mode(op), param(v, pos + 1, p1Mode(op), rel) * param(v, pos + 2, p2Mode(op), rel), rel)
                pos += 4
                break
            case 5:
                if (param(v, pos + 1, p1Mode(op), rel) != 0) {
                    pos = param(v, pos + 2, p2Mode(op), rel)
                } else {
                    pos += 3
                }
                break
            case 6:
                if (param(v, pos + 1, p1Mode(op), rel) == 0) {
                    pos = param(v, pos + 2, p2Mode(op), rel)
                } else {
                    pos += 3
                }
                break
            case 7:
                if (param(v, pos + 1, p1Mode(op), rel) < param(v, pos + 2, p2Mode(op), rel)) {
                    assignTo(v, pos + 3, p3Mode(op), 1, rel)
                } else {
                    assignTo(v, pos + 3, p3Mode(op), 0, rel)
                }
                pos += 4
                break
            case 8:
                if (param(v, pos + 1, p1Mode(op), rel) == param(v, pos + 2, p2Mode(op), rel)) {
                    assignTo(v, pos + 3, p3Mode(op), 1, rel)
                } else {
                    assignTo(v, pos + 3, p3Mode(op), 0, rel)
                }
                pos += 4
                break
            case 9:
                long shift = param(v, pos + 1, p1Mode(op), rel)
                rel += shift
                pos += 2
                break
            case 3:
                if (s.input.empty) {
                    s.v = v
                    s.pos = pos
                    s.rel = rel
                    //println("Empty input")
                    return
                }
                //println "Get input from $s.input"
                assignTo(v, pos + 1, p1Mode(op), s.input.poll(), rel)
                pos += 2
                break
            case 4:
                long out = param(v, pos + 1, p1Mode(op), rel)
                //println "Out: $out"
                output.offer(out)
                pos += 2
                break
            default:
                println "ERROR"
                throw new NullPointerException()
        }
    }
}


@Canonical
class State {
    Map<Long, Long> v
    int pos
    LinkedList<Long> input
    boolean ended = false
    int rel = 0
}

Queue output = new LinkedList<Long>()
Queue inputQ = new LinkedList<Long>()
State state = new State(
        v: v,
        pos: 0,
        input: inputQ,
)
//==============================================

def printBoard(Map panel) {
    for (int i = panel.keySet().collect { it[1] }.min(); i <= panel.keySet().collect { it[1] }.max(); ++i) {
        for (int j = panel.keySet().collect { it[0] }.min(); j <= panel.keySet().collect { it[0] }.max(); ++j) {
            if (i == 0 && j == 0) {
                print('S')
                continue
            }
            Integer val = panel[[j, i]]
            if (val == null) {
                print(' ')
            } else if (val == 0) {
                print('#')
            } else if (val == 1) {
                print('.')
            } else if (val == 2) {
                print('O')
            }
        }
        println()
    }
}

List<Integer> nextPos(List<Integer> curPos, int dir) {
    switch (dir) {
        case 1: return [curPos[0], curPos[1] - 1]
        case 2: return [curPos[0], curPos[1] + 1]
        case 3: return [curPos[0] - 1, curPos[1]]
        case 4: return [curPos[0] + 1, curPos[1]]
    }
}

int opposite(int dir) {
    switch (dir) {
        case 1: return 2
        case 2: return 1
        case 3: return 4
        case 4: return 3
    }
}

List<Integer> cur = [0, 0]
Map<List<Integer>, Integer> board = [:]
board[cur] = 1
Stack<Integer> path = []

while (true) {
    Map.Entry<List<String>, Integer> nextMove = [1, 3, 2, 4].collectEntries { [(nextPos(cur, it)): it] }.find {
        !(it.key in board.keySet())
    }
    if (nextMove == null && path.empty()) {
        break
    }
    if (nextMove == null) {
        int prev = path.pop()
        int op = opposite(prev)
        inputQ.offer(op)
        program(state, output)
        output.poll()
        cur = nextPos(cur, op)
    } else {
        inputQ.offer(nextMove.value)
        program(state, output)
        int out = output.poll()
        board[nextMove.key] = out
        if (out != 0) {
            cur = nextMove.key
            path.push(nextMove.value)
        }
    }
}

printBoard(board)

//=====================================
int manhattan(List<Integer> a, List<Integer> b) {
    (a[0] - b[0]).abs() + (a[1] - b[1]).abs()
}

List<Integer> dest = board.find { it.value == 2 }.key

Set<List<Integer>> visited = [cur] as Set
PriorityQueue<List<Integer>> pq = new PriorityQueue<>({ a, b ->
    if (a[0] == b[0]) {
        if (a[1] == b[1]) {
            return a[2] - b[2]
        }
        return a[1] - b[1]
    }
    return a[0] - b[0]
})

int minutes = -1
pq.offer([0, dest[0], dest[1]])
while (!pq.empty) {
    List<Integer> c = pq.poll()
    minutes = c[0]
    visited << c.subList(1, 3)
    List<List<Integer>> nexts = [1, 2, 3, 4].collect { nextPos(c.subList(1, 3), it) }.findAll {
        !(it in visited) && it in board.keySet() && board[it] != 0
    }
    nexts.each {
        pq.offer([c[0] + 1, it[0], it[1]])
    }
}
println("Fill with oxygen in $minutes minutes")