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

def program(State s, Queue<Long> output) {
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


@groovy.transform.Canonical
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

v[0L] = 2L
program(state, output)

def createBoard(Map panel, Queue output){
while(output.size() > 0){
    int x = output.poll()
    int y = output.poll()
    int t = output.poll()
    panel[[x,y]]=t
}
return panel
}
Map<List<Integer>, Integer> panel = [:]
createBoard(panel, output)

def printBoard(Map panel){
for(int i = panel.keySet().collect {it[1]}.min(); i <= panel.keySet().collect {it[1]}.max(); ++i){
for(int j = panel.keySet().collect {it[0]}.min(); j <= panel.keySet().collect {it[0]}.max(); ++j){
   if(j == -1){
       if(i == 0){
           println('Score: ' + panel[[j,i]])
       }
       continue
   }
   Integer val = panel[[j,i]]
   if(val == null){
       continue
   }
   if(val == 0){
        print('.')
   }else if(val == 1){
       print('#')
   }else if (val == 2){
       print('B')
   }else if (val == 3){
       print('H')
   }else if (val == 4){
       print('O')
    }
}
println()    
}
}


printBoard(panel)
println()
inputQ.offer(1)

def findB(Map panel){
panel.find {it.value == 4}.key    
}
def findH(Map panel){
panel.find {it.value == 3}.key    
}

def prevB = findB(panel)
boolean hit = false
int i = 0
while(!state.ended){
    ++i
    //println('===============================================')
    program(state,output)
    createBoard(panel, output)
    //printBoard(panel)
    def curB = findB(panel)
    def curH = findH(panel)
    //println("Ball is on " + curB)
    //println("H is on " + curH)
    hit = prevB[1] < curB[1] && curB[1] +1 == curH[1]
    if(hit){
       // println("Will HIT")
    }
    if(curB[0] == curH[0]){
        inputQ.offer(0)
    }else if(curB[0] < prevB[0]){
        if(curH[0] == curB[0] - 1){
            inputQ.offer(0)
        }else if(curH[0] < curB[0] - 1){
            inputQ.offer(1)
        }else{
            inputQ.offer(-1)
        }
    }else{
        if(curH[0] == curB[0] + 1){
            inputQ.offer(0)
        }else if(curH[0] > curB[0] + 1){
            inputQ.offer(-1)
        }else{
            inputQ.offer(1)
        }
    }
    prevB = curB
}
//println(state.ended)
println("Iter: $i")
println(panel[[-1,0]])
