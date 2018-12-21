import groovy.transform.Canonical

static String readInput(String file) {
    return new File(file).text
}

static List<String> readInputAsLines(String file) {
    return readInput(file).trim().split('\n')
}

List<String> lines = readInputAsLines('input.txt')
//lines = readInputAsLines('other.txt')

@Canonical
class Operation {
    String name
    int a
    int b
    int c

    void apply(long[] registers) {
        switch (name) {
            case 'addr':
                registers[c] = registers[a] + registers[b]
                break
            case 'addi':
                registers[c] = registers[a] + b
                break
            case 'setr':
                registers[c] = registers[a]
                break
            case 'seti':
                registers[c] = a
                break
            case 'mulr':
                registers[c] = registers[a] * registers[b]
                break
            case 'muli':
                registers[c] = registers[a] * b
                break
            case 'banr':
                registers[c] = registers[a] & registers[b]
                break
            case 'bani':
                registers[c] = registers[a] & b
                break
            case 'borr':
                registers[c] = registers[a] | registers[b]
                break
            case 'bori':
                registers[c] = registers[a] | b
                break
            case 'gtir':
                registers[c] = a > registers[b] ? 1 : 0
                break
            case 'gtri':
                registers[c] = registers[a] > b ? 1 : 0
                break
            case 'gtrr':
                registers[c] = registers[a] > registers[b] ? 1 : 0
                break
            case 'eqir':
                registers[c] = a == registers[b] ? 1 : 0
                break
            case 'eqri':
                registers[c] = registers[a] == b ? 1 : 0
                break
            case 'eqrr':
                registers[c] = registers[a] == registers[b] ? 1 : 0
                break
        }
    }

    String toString() {
        switch (name) {
            case 'addr':
                return "r$c = r$a + r$b"
            case 'addi':
                return "r$c = r$a + $b"
            case 'setr':
                return "r$c = r$a"
            case 'seti':
                return "r$c = $a"
            case 'mulr':
                return "r$c = r$a * r$b"
            case 'muli':
                return "r$c = r$a * $b"
            case 'banr':
                return "r$c = r$a & r$b"
            case 'bani':
                return "r$c = r$a & $b"
            case 'borr':
                return "r$c = r$a | r$b"
            case 'bori':
                return "r$c = r$a | $b"
            case 'gtir':
                return "r$c = $a > r$b? 1 : 0"
            case 'gtri':
                return "r$c = r$a > $b? 1 : 0"
            case 'gtrr':
                return "r$c = r$a > r$b? 1 : 0"
            case 'eqir':
                return "r$c = $a == r$b? 1 : 0"
            case 'eqri':
                return "r$c = r$a == $b? 1 : 0"
            case 'eqrr':
                return "r$c = r$a == r$b? 1 : 0"
        }
    }
}

int ip = lines[0].split(/ /)[-1] as int
List<Operation> instructions = []
for (int i = 1; i < lines.size(); ++i) {
    String[] line = lines[i].split(/ /)
    instructions << new Operation(name: line[0], a: line[1] as int, b: line[2] as int, c: line[3] as int)
}

instructions.eachWithIndex { it, i -> println("$i: $it") }

int threshold = 10000000
int curMinR4 = 10000000000
long operations = 0
Map<Long, Long> result2Iteration = [:]
for (int i = 0; i < 1; ++i) {
    println(i)
    long[] registers = [i, 0, 0, 0, 0, 0, 0]
    def count
    Map<Integer, Integer> instrToCount = [:].withDefault { 0 }
    while (true) {
        ++operations
        int cur = registers[6]
        registers[ip] = registers[6]
//        println("$cur: ${instructions[cur]}")
//        print("Before $registers || ")
//        if (cur == 20 && registers[2] <= registers[3]) {
//            while ((registers[5] + 1) * 256 < registers[3]) {
//                operations += 7
//                ++registers[5]
//            }
//        }
        instrToCount[cur]++
        instructions[cur].apply(registers)
//        println(registers)
        if (cur == 28) {
            Long current = result2Iteration[registers[4]]
            if(current == null){
                result2Iteration[registers[4]] = operations
                println("Max: ${result2Iteration.max {it.value}.key}")
            }
            if(operations > 100000000000){
                println("Max: ${result2Iteration.max {it.value}.key}")
            }
        }
        if (registers[ip] + 1 >= instructions.size()) {
            println("Halts")
            throw new RuntimeException("WIn $i")
        }
        registers[6] = registers[ip] + 1
    }
}
//
//println("Checking nums:")
//List<Integer> nums = r4s.sort()
//threshold = 100
//println(nums)
//for (int i : nums) {
//    println(i)
//    int[] registers = [i, 0, 0, 0, 0, 0, 0]
//    def count
//    Map<Integer, Integer> instrToCount = [:].withDefault { 0 }
//    while (true) {
//        int cur = registers[6]
//        registers[ip] = registers[6]
//        if (cur == 28) {
////            r4s << registers[4]
////            println(instructions[cur])
////            print("Before $registers || ")
//        }
//        instrToCount[cur]++
//        instructions[cur].apply(registers)
//        if (cur == 28) {
////            println(registers)
//        }
//        if (registers[ip] + 1 >= instructions.size()) {
//            break
//        }
//        registers[6] = registers[ip] + 1
//        count = instrToCount[28] > threshold
//        if (count) {
//            println("Break $i: is in loop")
//            break
//        }
//    }
//    if (!count) {
//        break
//    }
//}
//println(r4s.min())
