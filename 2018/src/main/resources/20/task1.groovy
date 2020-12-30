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
class Pos {
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
}

Stack<Pos> cross = []
Map<Pos, Integer> posToDist = [:]
Pos curPos = new Pos(0, 0)
posToDist[curPos] = 0
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
        int distance = posToDist[curPos]
        curPos = curPos."$c"()
        if (posToDist[curPos]) {
            if (posToDist[curPos] > distance + 1) {
                posToDist[curPos] = distance + 1
            }
        } else {
            posToDist[curPos] = distance + 1
        }
    }
}

println(posToDist.values().max())
println(posToDist.values().count { it >= 1000 })
