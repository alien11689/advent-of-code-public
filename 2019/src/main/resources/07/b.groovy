List<Long> v = new File('input.txt').text.split(',').collect { it as long } as ArrayList
//v = new File('inputDemo1.txt').text.split(',').collect { it as long} as ArrayList

def program(State s, Queue output) {
    def v = s.v
    int pos = s.pos

    while (true) {
        int op = v[pos]
        if (op == 99L) {
            s.ended = true
            return
        }
        if (op == 1) {
            v[v[pos + 3]] = v[v[pos + 1]] + v[v[pos + 2]]
            pos += 4
        } else if (op == 1101) {
            v[v[pos + 3]] = v[pos + 1] + v[pos + 2]
            pos += 4
        } else if (op == 101) {
            v[v[pos + 3]] = v[pos + 1] + v[v[pos + 2]]
            pos += 4
        } else if (op == 1001) {
            v[v[pos + 3]] = v[v[pos + 1]] + v[pos + 2]
            pos += 4
        } else if (op == 2) {
            v[v[pos + 3]] = v[v[pos + 1]] * v[v[pos + 2]]
            pos += 4
        } else if (op == 1002) {
            v[v[pos + 3]] = v[v[pos + 1]] * v[pos + 2]
            pos += 4
        } else if (op == 1102) {
            v[v[pos + 3]] = v[pos + 1] * v[pos + 2]
            pos += 4
        } else if (op == 102) {
            v[v[pos + 3]] = v[pos + 1] * v[v[pos + 2]]
            pos += 4
        } else if (op == 1105) {
            if (v[pos + 1] != 0) {
                pos = v[pos + 2]
            } else {
                pos += 3
            }
        } else if (op == 105) {
            if (v[pos + 1] != 0) {
                pos = v[v[pos + 2]]
            } else {
                pos += 3
            }
        } else if (op == 1005) {
            if (v[v[pos + 1]] != 0) {
                pos = v[pos + 2]
            } else {
                pos += 3
            }
        } else if (op == 1106) {
            if (v[pos + 1] == 0) {
                pos = v[pos + 2]
            } else {
                pos += 3
            }
        } else if (op == 106) {
            if (v[pos + 1] == 0) {
                pos = v[v[pos + 2]]
            } else {
                pos += 3
            }
        } else if (op == 1006) {
            if (v[v[pos + 1]] == 0) {
                pos = v[pos + 2]
            } else {
                pos += 3
            }
        } else if (op == 7) {
            if (v[v[pos + 1]] < v[v[pos + 2]]) {
                v[v[pos + 3]] = 1
            } else {
                v[v[pos + 3]] = 0
            }
            pos += 4
        } else if (op == 107) {
            if (v[pos + 1] < v[v[pos + 2]]) {
                v[v[pos + 3]] = 1
            } else {
                v[v[pos + 3]] = 0
            }
            pos += 4
        } else if (op == 1107) {
            if (v[pos + 1] < v[pos + 2]) {
                v[v[pos + 3]] = 1
            } else {
                v[v[pos + 3]] = 0
            }
            pos += 4
        } else if (op == 1007) {
            if (v[v[pos + 1]] < v[pos + 2]) {
                v[v[pos + 3]] = 1
            } else {
                v[v[pos + 3]] = 0
            }
            pos += 4
        } else if (op == 8) {
            if (v[v[pos + 1]] == v[v[pos + 2]]) {
                v[v[pos + 3]] = 1
            } else {
                v[v[pos + 3]] = 0
            }
            pos += 4
        } else if (op == 1108) {
            if (v[pos + 1] == v[pos + 2]) {
                v[v[pos + 3]] = 1
            } else {
                v[v[pos + 3]] = 0
            }
            pos += 4
        } else if (op == 1008) {
            if (v[v[pos + 1]] == v[pos + 2]) {
                v[v[pos + 3]] = 1
            } else {
                v[v[pos + 3]] = 0
            }
            pos += 4
        } else if (op == 108) {
            if (v[pos + 1] == v[v[pos + 2]]) {
                v[v[pos + 3]] = 1
            } else {
                v[v[pos + 3]] = 0
            }
            pos += 4
        } else if (op == 3) {
            if (s.input.empty) {
                s.v = v
                s.pos = pos
                return
            }
            println "Get input from $s.input"
            v[v[pos + 1]] = s.input.poll()
            pos += 2
        } else if (op == 104) {
            int out = v[pos + 1]
            println "Out: $out"
            output.offer(out)
            pos += 2
        } else if (op == 4) {
            int out = v[v[pos + 1]]
            println "Out: $out"
            output.offer(out)
            pos += 2
        } else {
            println "Unknown $op"
            return
        }
    }
}


Set<List<Integer>> phasesSet = (5..9).permutations()

@groovy.transform.Canonical
class State {
    List<Long> v
    int pos
    LinkedList<Long> input
    boolean ended = false
}

println(phasesSet.collect { phases ->
    println "Checking $phases"
    Map<Integer, State> states = [:]
    for (int i = 0; i < phases.size(); ++i) {
        states[i] = new State(
                v: v.clone(),
                pos: 0,
                input: new LinkedList([phases[i]])
        )
    }
    states[0].input.offer(0)
    int cur = 0
    while (!states.values().every { it.ended }) {
        int nextCur = (cur + 1) % phases.size()
        program(states[cur], states[nextCur].input)
        cur = nextCur
    }
    int signal = states[0].input.poll()
    println "$phases -> $signal"
    signal
}.max())
