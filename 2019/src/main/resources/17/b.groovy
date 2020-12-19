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

state.v[0L] = 2L

program(state, output)
println("RUN")

int R = 82
int L = 76
int A = 65
int B = 66
int C = 67
int coma = 44
int _8 = 56
int _4 = 52
int _6 = 54
int _1 = 49
int _0 = 48
int nl = 10
int n = 110
int y = 79

inputQ.offer(B)
inputQ.offer(coma)
inputQ.offer(B)
inputQ.offer(coma)
inputQ.offer(C)
inputQ.offer(coma)
inputQ.offer(A)
inputQ.offer(coma)
inputQ.offer(C)
inputQ.offer(coma)
inputQ.offer(A)
inputQ.offer(coma)
inputQ.offer(C)
inputQ.offer(coma)
inputQ.offer(A)
inputQ.offer(coma)
inputQ.offer(B)
inputQ.offer(coma)
inputQ.offer(A)
inputQ.offer(nl)

//A
inputQ.offer(L)
inputQ.offer(coma)
inputQ.offer(_8)
inputQ.offer(coma)
inputQ.offer(R)
inputQ.offer(coma)
inputQ.offer(_6)
inputQ.offer(coma)
inputQ.offer(L)
inputQ.offer(coma)
inputQ.offer(_1)
inputQ.offer(_0)
inputQ.offer(coma)
inputQ.offer(L)
inputQ.offer(coma)
inputQ.offer(_1)
inputQ.offer(_0)
inputQ.offer(nl)

// B
inputQ.offer(R)
inputQ.offer(coma)
inputQ.offer(_6)
inputQ.offer(coma)
inputQ.offer(L)
inputQ.offer(coma)
inputQ.offer(_8)
inputQ.offer(coma)
inputQ.offer(R)
inputQ.offer(coma)
inputQ.offer(_8)
inputQ.offer(nl)

//C
inputQ.offer(R)
inputQ.offer(coma)
inputQ.offer(_4)
inputQ.offer(coma)
inputQ.offer(R)
inputQ.offer(coma)
inputQ.offer(_6)
inputQ.offer(coma)
inputQ.offer(R)
inputQ.offer(coma)
inputQ.offer(_6)
inputQ.offer(coma)
inputQ.offer(R)
inputQ.offer(coma)
inputQ.offer(_4)
inputQ.offer(coma)
inputQ.offer(R)
inputQ.offer(coma)
inputQ.offer(_4)
inputQ.offer(nl)

//mode
inputQ.offer(n)
inputQ.offer(nl)

program(state, output)

Map<List<Integer>, Integer> m = [:]
int i =0
int j = 0
int last = -1
while (!output.empty) {
    int cur = output.poll()
    last = cur
    switch (cur) {
        case 35: print('#'); m[[i++, j]] = cur; break
        case 46: print('.'); m[[i++, j]] = cur; break
        case 94: print('^'); m[[i++, j]] = cur; break
        case 60: print('<'); m[[i++, j]] = cur; break
        case 62: print('>'); m[[i++, j]] = cur; break
        case 118: print('v'); m[[i++, j]] = cur; break
        case 10: println(); j++; i = 0; break
        default: print(cur as char)
    }
}
println("Ended: ${state.ended} => $last")
