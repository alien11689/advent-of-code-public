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
input = '^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$'

List<String> buildPaths(String input) {
    Stack<List<String>> stack = []
    List<String> result = null
    input.substring(1).each { c ->
//        println(c)
        if (c == 'E' || c == 'W' || c == 'N' || c == 'S' || c == '(') {
            stack.push([c])
        } else if (c == '|') {
            if (stack.peek() == ['|'] || stack.peek() == ['(']) {
                stack.push([''])
            }
            stack.push([c])
        } else if (c == ')') {
            List<String> paths = []
            List<String> curPath = ['']
            while (stack.peek() != ['(']) {
                List<String> prev = stack.pop()
                if (prev == ['|']) {
                    paths.addAll(curPath)
                    curPath = ['']
                } else {
                    curPath = prev.collectMany { p -> curPath.collect { "$p$it" } }.flatten()
                }
            }
            paths.addAll(curPath)
            stack.pop() // Remove (
            stack.push(paths)
        } else if (c == '$') {
            List<String> curPath = ['']
            while (!stack.empty()) {
                List<String> prev = stack.pop()
                curPath = prev.collectMany { p -> curPath.collect { "$p$it" } }.flatten()
            }
            result = curPath
        }
        println(stack.size())
    }
    return result
}

List<String> paths = buildPaths(input)
println(paths)

@Canonical
class Pos {
    int x
    int y

    Pos e() {
        new Pos(x + 1, y)
    }

    Pos w() {
        new Pos(x - 1, y)
    }

    Pos n() {
        new Pos(x, y - 1)
    }

    Pos s() {
        new Pos(x, y + 1)
    }
}

Map<Pos, String> map = [:]


paths.each { String path ->
    Pos curPos = new Pos(0, 0)
    map[curPos] = 'X'
    path.each { c ->
        switch (c) {
            case 'E':
                Pos next = curPos.e()
                map[next] = '|'
                curPos = next.e()
                map[curPos] = '.'
                break
            case 'W':
                Pos next = curPos.w()
                map[next] = '|'
                curPos = next.w()
                map[curPos] = '.'
                break
            case 'N':
                Pos next = curPos.n()
                map[next] = '-'
                curPos = next.n()
                map[curPos] = '.'
                break
            case 'S':
                Pos next = curPos.s()
                map[next] = '-'
                curPos = next.s()
                map[curPos] = '.'
                break
        }
    }
}

for (int y = map.keySet().min { it.y }.y - 1; y <= map.keySet().max { it.y }.y + 1; ++y) {
    for (int x = map.keySet().min { it.x }.x - 1; x <= map.keySet().max { it.x }.x + 1; ++x) {
        print(map.getOrDefault(new Pos(x, y), '#'))
    }
    println()
}