import groovy.transform.Canonical

static String readInput(String file) {
    return new File(file).text
}

static List<String> readInputAsLines(String file) {
    return readInput(file).trim().split('\n')
}

String input = readInput('input.txt').trim()
//input = '^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$'
//input = '^S(E|W(N|WS|)N)E$'
//input = '^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$'
//input = '^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$'


@Canonical
class Pos implements Comparable<Pos> {
    int x
    int y

    Pos E() {
        new Pos(x + 1, y)
    }

    Pos W() {
        new Pos(x - 1, y)
    }

    Pos N() {
        new Pos(x, y - 1)
    }

    Pos S() {
        new Pos(x, y + 1)
    }

    @Override
    int compareTo(Pos o) {
        if (y == o.y) {
            return x <=> o.x
        }
        return y <=> o.y
    }
}

Map<Pos, String> pos2Char = [:]
Stack<Pos> cross = []
Pos curPos = new Pos(0, 0)
input.substring(1).each { c ->
//    println(c)
    if (c == '(') {
        cross.push(curPos)
    } else if (c == '$') {
    } else if (c == ')') {
        curPos = cross.pop()
    } else if (c == '|') {
        curPos = cross.peek()
    } else {
        curPos = curPos."$c"()
        pos2Char[curPos] = c == 'N' || c == 'S' ? '-' : '|'
        curPos = curPos."$c"()
        pos2Char[curPos] = '.'
    }
}
pos2Char[new Pos(0, 0)] = 'X'

//for (int y = pos2Char.keySet().min { it.y }.y - 1; y <= pos2Char.keySet().max { it.y }.y + 1; ++y) {
//    for (int x = pos2Char.keySet().min { it.x }.x - 1; x <= pos2Char.keySet().max { it.x }.x + 1; ++x) {
//        print(pos2Char.getOrDefault(new Pos(x, y), '#'))
//    }
//    println()
//}

@Canonical
class Dist implements Comparable<Dist> {
    Pos pos
    int doors

    @Override
    int compareTo(Dist o) {
        if (o.doors != doors) {
            return doors <=> o.doors
        }
        return pos <=> o.pos
    }
}

PriorityQueue<Dist> toCheck = new PriorityQueue<Dist>()
Map<Pos, Integer> memory = [:]

toCheck.offer(new Dist(new Pos(0, 0), 0))
int maxDoors = 0
while (!toCheck.empty) {
    Dist cur = toCheck.poll()
    if (cur.pos in memory.keySet()) {
        continue
    }
    maxDoors = maxDoors < cur.doors ? cur.doors : maxDoors
    memory[cur.pos] = cur.doors
    if (pos2Char[cur.pos.E()] == '|') {
        Pos pos = cur.pos.E().E()
        toCheck.offer(new Dist(pos, cur.doors + 1))
    }
    if (pos2Char[cur.pos.W()] == '|') {
        Pos pos = cur.pos.W().W()
        toCheck.offer(new Dist(pos, cur.doors + 1))
    }
    if (pos2Char[cur.pos.N()] == '-') {
        Pos pos = cur.pos.N().N()
        toCheck.offer(new Dist(pos, cur.doors + 1))
    }
    if (pos2Char[cur.pos.S()] == '-') {
        Pos pos = cur.pos.S().S()
        toCheck.offer(new Dist(pos, cur.doors + 1))
    }
}
println maxDoors
println(memory.values().count { it >= 1000 })
