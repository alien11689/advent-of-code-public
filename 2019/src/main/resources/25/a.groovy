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
//                println "Out: $out"
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

//class Computer {
//    Computer(int address, Map<Long, Long> v) {
//        this.address = address
//        this.output = new LinkedList<Long>()
//        this.inputQ = new LinkedList<Long>()
//        this.inputQ.offer(address)
//        this.state = new State(
//                v: v.findAll { it }.withDefault { 0L },
//                pos: 0,
//                input: inputQ,
//        )
//    }
//
//    int address
//    Queue output
//    Queue inputQ
//    State state
//}

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


void runInteractive(Map<Long,Long> v) {
    Queue output = new LinkedList<Long>()
    Queue inputQ = new LinkedList<Long>()
    State state = new State(
            v: v.collectEntries { it }.withDefault { 0L },
            pos: 0,
            input: inputQ,
    )
    State saved = null
    while (true) {
        program(state, output)

        while (!output.empty) {
            int cur = output.poll()
            switch (cur) {
                case 10: println(); break
                default: print(cur as char)
            }
        }
        String command = System.console().readLine()
        switch (command) {
            case 'e': command = 'east'; break
            case 's': command = 'south'; break
            case 'n': command = 'north'; break
            case 'w': command = 'west'; break
            case 'save': saved = new State(
                    v: state.v.collectEntries { it }.withDefault { 0L },
                    pos: state.pos,
                    input: inputQ,
                    rel: state.rel
            ); println "saved"; break
            case 'load': state = saved; println("loaded"); break
            default: break
        }
        instruction(inputQ, command)
    }
}

void solve(Map<Long,Long> v) {
    Queue output = new LinkedList<Long>()
    Queue inputQ = new LinkedList<Long>()
    State state = new State(
            v: v.collectEntries { it }.withDefault { 0L },
            pos: 0,
            input: inputQ,
    )
    [
            'south',
            'west',
            'take hologram',
            'south',
            'west',
            'west',
            'take hypercube',
            'east',
            'east',
            'north',
            'east',
            'south',
            'west',
            'north',
            'take coin',
            'south',
            'east',
            'take cake',
            'east',
            'south',
            'east',
            'south',
            'south',
    ].each {
        instruction(inputQ, it)
    }
    program(state, output)
    while (!output.empty) {
        int cur = output.poll()
        switch (cur) {
            case 10: println(); break
            default: print(cur as char)
        }
    }
}

//runInteractive(v)
solve(v)

return

//photons > X
//molten lava > X
//food ration > X
//astrolabe > X
//space law space brochure > X
//
//cake < X
//escape pod < X
//hypercube < X
//hologram < X
//wreath < X
//coin < X
//
//hologram + cake + wreath + coin < X
//hologram + cake + wreath + coin + hypercube > X
//coin < hypercube
//
//Solution:
//s
//w
//take hologram
//s
//w
//w
//take hypercube
//e
//e
//n
//e
//s
//w
//n
//take coin
//s
//e
//take cake
//e
//s
//e
//s
//s
