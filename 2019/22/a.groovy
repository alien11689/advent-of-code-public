String input = 'input.txt'
List<List<String>> lines = readInput(input)

private List<List<String>> readInput(String input) {
    new File(input).text.split('\n').collect { it.split(' ') as List }
}

Map<Integer, Integer> buildMap(int size) {
    Map<Integer, Integer> m = [:]
    size.times {
        m[it] = it
    }
    return m
}

Map<Integer, Integer> apply(List<String> instr, Map<Integer, Integer> m) {
    if (instr[0] == 'cut') { // cut X
        return cut(instr[1] as int, m)
    }
    if (instr[3] == 'stack') { //deal into new stack
        return dealIntoNewStack(m)
    }
    if (instr[2] == 'increment') { // deal with increment X
        return dealWithIncrement(instr[3] as int, m)
    }
}

Map<Integer, Integer> dealIntoNewStack(Map<Integer, Integer> m) {
    int i = 0
    return m.entrySet().sort { -it.key }.collectEntries { [(i++): it.value] }
}

//println(dealIntoNewStack(buildMap(10)))

Map<Integer, Integer> dealWithIncrement(int incr, Map<Integer, Integer> m) {
    int i = 0
    return m.entrySet().sort { it.key }.collectEntries {
        int now = i
        i = (i + incr) % m.size()
        [(now): it.value]
    }
}

//println(dealWithIncrement(3, buildMap(10)))

Map<Integer, Integer> cut(int point, Map<Integer, Integer> m) {
    int minIndex = m.keySet().min()
    int maxIndex = m.keySet().max()
    if (point > 0) {
        point.times {
            int v = m[minIndex]
            m.remove(minIndex)
            ++minIndex
            m[maxIndex + 1] = v
            ++maxIndex
        }
    } else {
        point.abs().times {
            int v = m[maxIndex]
            m.remove(maxIndex)
            --maxIndex
            m[minIndex - 1] = v
            --minIndex
        }
    }
    return m
}

void printWhole(Map<Integer, Integer> m) {
    println(m.entrySet().sort { it.key }.collect { it.value }.join(','))
}

Map<Integer, Integer> inOrder(Map<Integer, Integer> m) {
    int i = 0
    return m.entrySet().sort { it.key }.collectEntries {
        int now = i
        i = (i + 1) % m.size()
        [(now): it.value]
    }
}

printWhole(readInput('input1.txt').inject(buildMap(10)) { Map<Integer, Integer> m, inp -> apply(inp, m) })
printWhole(readInput('input2.txt').inject(buildMap(10)) { Map<Integer, Integer> m, inp -> apply(inp, m) })
printWhole(readInput('input3.txt').inject(buildMap(10)) { Map<Integer, Integer> m, inp -> apply(inp, m) })
printWhole(readInput('input4.txt').inject(buildMap(10)) { Map<Integer, Integer> m, inp -> apply(inp, m) })

Map res = readInput('input.txt').inject(buildMap(10007)) { Map<Integer, Integer> m, inp -> apply(inp, m) }
println(inOrder(res).find {it.value == 2019})