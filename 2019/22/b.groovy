private List<List<String>> readInput(String input) {
    new File(input).text.split('\n').collect { it.split(' ') as List }
}

Map<Long, Long> buildMap(long size) {
    Map<Long, Long> m = [:]
    size.times {
        m[it] = it
    }
    return m
}

Map<Long, Long> apply(List<String> instr, Map<Long, Long> m) {
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

Map<Long, Long> dealIntoNewStack(Map<Long, Long> m) {
    long i = 0
    return m.entrySet().sort { -it.key }.collectEntries { [(i++): it.value] }
}

//println(dealIntoNewStack(buildMap(10)))

Map<Long, Long> dealWithIncrement(int incr, Map<Long, Long> m) {
    long i = 0
    return m.entrySet().sort { it.key }.collectEntries {
        long now = i
        i = (i + incr) % m.size()
        [(now): it.value]
    }
}

//println(dealWithIncrement(3, buildMap(10)))

Map<Long, Long> cut(int point, Map<Long, Long> m) {
    long minIndex = m.keySet().min()
    long maxIndex = m.keySet().max()
    if (point > 0) {
        point.times {
            long v = m[minIndex]
            m.remove(minIndex)
            ++minIndex
            m[maxIndex + 1] = v
            ++maxIndex
        }
    } else {
        point.abs().times {
            long v = m[maxIndex]
            m.remove(maxIndex)
            --maxIndex
            m[minIndex - 1] = v
            --minIndex
        }
    }
    return m
}

void printWhole(Map<Long, Long> m) {
    println(m.entrySet().sort { it.key }.collect { it.value }.join(','))
}

Map<Long, Long> inOrder(Map<Long, Long> m) {
    long i = 0
    return m.entrySet().sort { it.key }.collectEntries {
        long now = i
        i = (i + 1) % m.size()
        [(now): it.value]
    }
}

//printWhole(readInput('input1.txt').inject(buildMap(10)) { Map<Long,Long> m, inp -> apply(inp, m) })
//printWhole(readInput('input2.txt').inject(buildMap(10)) { Map<Long,Long> m, inp -> apply(inp, m) })
//printWhole(readInput('input3.txt').inject(buildMap(10)) { Map<Long,Long> m, inp -> apply(inp, m) })
//printWhole(readInput('input4.txt').inject(buildMap(10)) { Map<Long,Long> m, inp -> apply(inp, m) })
//
List<List<String>> input = readInput('input.txt')
Map res = input.inject(buildMap(10007)) { Map<Long, Long> m, inp ->
    println("Applying $inp (2421 is on ${m.find { it.value == 7818L }.key})")
    Map<Long, Long> newM = inOrder(apply(inp, m))
    println("Now is on ${newM.find { it.value == 7818L }.key}")
    newM
}
println(res[2020L])

// ==================================
List<List<String>> reversedInput = input.reverse()

long size = 10007

long mod(long a, long b) {
    long r = a % b;
    return r < 0 ? r + b : r;
}

long applyReversed(List<String> instr, long cur, long size) {
    if (instr[0] == 'cut') { // cut X
        int cut = instr[1] as int
        return mod(cur + cut, size)
    }
    if (instr[3] == 'stack') { //deal into new stack
        return size - 1 - cur
    }
    if (instr[2] == 'increment') { // deal with increment X
        long inc = instr[3] as int
        int i = 0
        while (true) {
            long val = cur + (i * size)
            if (val % inc == 0) {
                return val / inc
            }
            ++i
        }
//        return dealWithIncrement(instr[3] as int, m)
    }
}

long res2 = reversedInput.inject(2020L) { long cur, List<String> inp ->
    long v = applyReversed(inp, cur, size)
    println("Cur $cur reverse to $v after $inp")
    v
}
println(res2)
//long size = 119315717514047
//long times = 101741582076661
//
//Map res = buildMap(119315717514047)