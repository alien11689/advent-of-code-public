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

    void apply(int[] registers) {
        int ip = registers[6]

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
}

int ip = lines[0].split(/ /)[-1] as int
int[] registers = [0, 0, 0, 0, 0, 0, 0]
List<Operation> instructions = []
for (int i = 1; i < lines.size(); ++i) {
    String[] line = lines[i].split(/ /)
    instructions << new Operation(name: line[0], a: line[1] as int, b: line[2] as int, c: line[3] as int)
}

while (true) {
    int cur = registers[6]
    registers[ip] = registers[6]
    println(instructions[cur])
    print("Before $registers || ")
    instructions[cur].apply(registers)
    println(registers)
    if (registers[ip] + 1 >= instructions.size()) {
        break
    }
    registers[6] = registers[ip] + 1
}
println(registers[0])