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
//List<String> buildPaths(String input) {
//    Stack<List<String>> stack = []
//    List<String> result = null
//    StringBuilder bufor = new StringBuilder()
//    input.substring(1).each { c ->
////        println(c)
//        if (c == 'E' || c == 'W' || c == 'N' || c == 'S') {
//            bufor.append(c)
//        } else if (c == '(') {
//            stack.push([bufor.toString()])
//            bufor = new StringBuilder()
//            stack.push([c])
//        } else if (c == '|') {
//            stack.push([bufor.toString()])
//            bufor = new StringBuilder()
//            if (stack.peek() == ['|'] || stack.peek() == ['(']) {
//                stack.push([''])
//            }
//            stack.push([c])
//        } else if (c == ')') {
//            stack.push([bufor.toString()])
//            bufor = new StringBuilder()
//            List<String> paths = []
//            List<String> curPath = ['']
//            while (stack.peek() != ['(']) {
//                List<String> prev = stack.pop()
//                if (prev == ['|']) {
//                    paths.addAll(curPath)
//                    curPath = ['']
//                } else {
//                    curPath = prev.collectMany { p -> curPath.collect { "$p$it" } }.flatten()
//                }
//            }
//            paths.addAll(curPath)
//            stack.pop() // Remove (
//            stack.push(paths)
//        } else if (c == '$') {
//            List<String> curPath = ['']
//            while (!stack.empty()) {
//                List<String> prev = stack.pop()
//                curPath = prev.collectMany { p -> curPath.collect { "$p$it" } }.flatten()
//            }
//            result = curPath
//        }
//        println(stack.size())
//    }
//    return result
//}
//
//List<String> paths = buildPaths(input)
//println(paths)
//
//
//
//Map<Pos, String> map = [:]
//
//
//paths.each { String path ->
//    Pos curPos = new Pos(0, 0)
//    map[curPos] = 'X'
//    path.each { c ->
//        switch (c) {
//            case 'E':
//                Pos next = curPos.e()
//                map[next] = '|'
//                curPos = next.e()
//                map[curPos] = '.'
//                break
//            case 'W':
//                Pos next = curPos.w()
//                map[next] = '|'
//                curPos = next.w()
//                map[curPos] = '.'
//                break
//            case 'N':
//                Pos next = curPos.n()
//                map[next] = '-'
//                curPos = next.n()
//                map[curPos] = '.'
//                break
//            case 'S':
//                Pos next = curPos.s()
//                map[next] = '-'
//                curPos = next.s()
//                map[curPos] = '.'
//                break
//        }
//    }
//}
//
//for (int y = map.keySet().min { it.y }.y - 1; y <= map.keySet().max { it.y }.y + 1; ++y) {
//    for (int x = map.keySet().min { it.x }.x - 1; x <= map.keySet().max { it.x }.x + 1; ++x) {
//        print(map.getOrDefault(new Pos(x, y), '#'))
//    }
//    println()
//}