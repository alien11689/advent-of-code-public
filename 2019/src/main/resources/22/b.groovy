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
//    println("Applying $inp (2421 is on ${m.find { it.value == 7818L }.key})")
    Map<Long, Long> newM = inOrder(apply(inp, m))
//    println("Now is on ${newM.find { it.value == 7818L }.key}")
    newM
}
println("Expected: " + res[2020L])

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
        return mod(size - 1 - cur, size)
    }
    if (instr[2] == 'increment') { // deal with increment X
        long inc = instr[3] as int
//        alternative
//        return (BigInteger.valueOf(inc).modInverse(size) * cur).mod(size)
        long i = 0
        while (true) {
            long val = cur + (i * size)
            if (val % inc == 0) {
                return val / inc
            }
            ++i
        }
    }

    // all operations are linear so it means that their combination is also linear -> y = ax+b
}

long applyIteration(List<List<String>> reversedInput, long toFind, long size) {
    return reversedInput.inject(toFind) { long cur, List<String> inp ->
        applyReversed(inp, cur, size)
    }
}

long cur = 2020
println("Quick test")

println("After iteration \t ${0}: \t $cur")
for (long i = 0; i < 2; ++i) {
    long prev = cur
    cur = applyIteration(reversedInput, cur, size)
    long diff = mod(cur - prev, size)
    println("After iteration \t ${i + 1}: \t $cur \t diff $diff ")
}
//println(cur)

size = 119315717514047
long times = 101741582076661
cur = 2020L

println("Checking first iter")

BigInteger[] xx = new BigInteger[3]
xx[0] = cur

println("After iteration \t ${0}: \t $cur")
for (long i = 0; i < 2; ++i) {
    cur = applyIteration(reversedInput, cur, size)
    xx[i + 1] = cur
    println("After iteration \t ${i + 1}: \t $cur")
}
println(xx)

// x2 = (a * x1 + b) % size
// x3 = (a * x2 + b) % size

// a = (x2 -x3)/(x1 -x2) => (x2 - x3) * (x1 - x2)^(-1)
// b = x3 - ax2

BigInteger a = (xx[1] - xx[2]) * ((xx[0] - xx[1]) as BigInteger).modInverse(size) % size
BigInteger b = (xx[1] - a * xx[0]) % size + size
println("a = $a")
println("b = $b")
println("f(2020) = ${(a * xx[0] + b) % size}")
println("f(f(2020)) = ${(a * xx[1] + b) % size}")

// y1 = ax + b
// y2 = a(ax + b) + b => a^2 * x + ab + b
// y3 = a(y2) + b => a^3 * x + a^2 * b + ab + b
// https://en.wikipedia.org/wiki/Geometric_series
// yn = a^n * x + sum (k=0->n-1) b*a^n => a^n * x + b(1-a^n)/(1 - a) => a^n * x + b(a^n - 1)/(a - 1) => a^n * x + b(a^n - 1) * (a - 1)^(-1)

// (a + b) mod n => (a mod n + b mod n) mod n
// (a * b) mod n => (a mod n * b mod n) mod n

BigInteger begin = xx[0] * (a.modPow(times as BigInteger, size as BigInteger))
println(begin)
BigInteger end = b * (a.modPow(times as BigInteger, size as BigInteger) - 1) * ((a - 1).modInverse(size))
println(end)
println((begin + end) % size)

// 53669811261199 is too low
// 114675948260108 is too high