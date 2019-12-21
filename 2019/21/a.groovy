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
                println "ERROR $op"
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

//Long uberProgram(Map v, int x, int y) {
//    Queue output = new LinkedList<Long>()
//    Queue inputQ = new LinkedList<Long>()
//    State state = new State(
//            v: v.collectEntries { it }.withDefault { 0L },
//            pos: 0,
//            input: inputQ,
//    )
//    inputQ.offer(x)
//    inputQ.offer(y)
//    program(state, output)
//    if (output.empty) {
//        return null
//    } else {
//        return output.poll()
//    }
//}
//==============================================

def instruction(Queue input, String text) {
    text.each {
        input.offer((it as char) as int)
    }
    input.offer(10)
}

[
        'NOT C J',
        'AND D J',
        'NOT A T',
        'OR T J'
//        'NOT B T',
//        'AND T J',
//        'NOT C T',
//        'AND T J',
//        'AND D J',
].each { instruction(inputQ, it) }

//instruction(inputQ, 'NOT A J')
//instruction(inputQ, 'OR D J')
//instruction(inputQ, 'NOT A T')
//instruction(inputQ, 'NOT D T')
//instruction(inputQ, 'NOT T J')
////instruction(inputQ, 'NOT A J')
instruction(inputQ, 'WALK')

program(state, output)

int i = 0
int j = 0

Map<List<Integer>, Integer> m = [:]

int last
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
println(last)
