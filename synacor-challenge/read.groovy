import groovy.transform.Canonical

List<Integer> readNums(int[] bytes) {
    List<Integer> instr = []
    for (int i = 0; i < bytes.size(); i += 2) {
        int a = bytes[i]
        int b = bytes[i + 1]
        instr << a + b * (2**8)
    }
    return instr
}

@Canonical
class Instr {
    int address
    String name
    int a
    int b
    int c


    Integer process(Stack<Integer> stack, int[] registers) {
        switch (name) {
            case 'halt': return -1
            case 'noop': break
            case 'out': print(a as char); break
            case 'jmp': return a
            case 'jt':
                if (valueOrRegisterValue(a, registers) > 0) {
                    return b
                }
                break
            case 'jf':
                if (valueOrRegisterValue(a, registers) == 0) {
                    return b
                }
                break
            case 'set':
                //println("SET $a $b")
                registers[a % 32768] = valueOrRegisterValue(b, registers)
                break
            case 'add':
                //println("ADD $a $b $c")
                registers[a % 32768] = (valueOrRegisterValue(b, registers) + valueOrRegisterValue(c, registers)) % 32768
                break
            case 'eq':
                //println("EQ $a $b $c -> ${valueOrRegisterValue(b, registers)} == ${valueOrRegisterValue(c, registers)}")
                registers[a % 32768] = valueOrRegisterValue(b, registers) == valueOrRegisterValue(c, registers) ? 1 : 0
                break
            case 'push':
                stack.push(valueOrRegisterValue(a, registers))
                break
            case 'pop':
                registers[a % 32768] = stack.pop()
                break
            case 'gt':
                registers[a % 32768] = valueOrRegisterValue(b, registers) > valueOrRegisterValue(c, registers) ? 1 : 0
                break
            case 'and':
                registers[a % 32768] = (valueOrRegisterValue(b, registers) & valueOrRegisterValue(c, registers)) % 32768
                break
            case 'or':
                registers[a % 32768] = (valueOrRegisterValue(b, registers) | valueOrRegisterValue(c, registers)) % 32768
                break
            case 'not':
                registers[a % 32768] = Integer.parseInt(String.format("%15s", Integer.toBinaryString(valueOrRegisterValue(b, registers)))
                        .replace(' ', '0')
                        .collect {
                    it == '0' ? '1' : '0'
                }.join(), 2) % 32768
                break
            default:
                throw new RuntimeException("Unknown instr $name")
        }
        return null
    }

    private int valueOrRegisterValue(int num, int[] registers) {
        num >= 32768 ? registers[num % 32768] : num
    }

    @Override
    public String toString() {
        return "$address: ${name.toUpperCase()} ${[a, b, c].join(' ')}";
    }
}

List<Instr> readInstr(List<Integer> nums) {
    int i = 0
    List<Instr> instructions = []
    StringBuilder message = new StringBuilder()
    while (i < nums.size()) {
        switch (nums[i]) {
            case 0: instructions << new Instr(i, 'halt'); break
            case 1: instructions << new Instr(i, 'set', nums[++i], nums[++i]); break
            case 2: instructions << new Instr(i, 'push', nums[++i]); break
            case 3: instructions << new Instr(i, 'pop', nums[++i]); break
            case 4: instructions << new Instr(i, 'eq', nums[++i], nums[++i], nums[++i]); break
            case 5: instructions << new Instr(i, 'gt', nums[++i], nums[++i], nums[++i]); break
            case 6: instructions << new Instr(i, 'jmp', nums[++i]); break
            case 7: instructions << new Instr(i, 'jt', nums[++i], nums[++i]); break
            case 8: instructions << new Instr(i, 'jf', nums[++i], nums[++i]); break
            case 9: instructions << new Instr(i, 'add', nums[++i], nums[++i], nums[++i]); break
            case 10: instructions << new Instr(i, 'mult', nums[++i], nums[++i], nums[++i]); break
            case 11: instructions << new Instr(i, 'mod', nums[++i], nums[++i], nums[++i]); break
            case 12: instructions << new Instr(i, 'and', nums[++i], nums[++i], nums[++i]); break
            case 13: instructions << new Instr(i, 'or', nums[++i], nums[++i], nums[++i]); break
            case 14: instructions << new Instr(i, 'not', nums[++i], nums[++i]); break
            case 15: instructions << new Instr(i, 'rmem', nums[++i], nums[++i]); break
            case 16: instructions << new Instr(i, 'wmem', nums[++i], nums[++i]); break
            case 17: instructions << new Instr(i, 'call', nums[++i]); break
            case 18: instructions << new Instr(i, 'ret'); break
            case 19: instructions << new Instr(i, 'out', nums[++i]); message.append(nums[i] as char); break
            case 20: instructions << new Instr(i, 'in', nums[++i]); break
            case 21: instructions << new Instr(i, 'noop'); break
            default: ++i; break// throw new RuntimeException("Unknown operation on $i: ${nums[i]}, ${nums[i+1]}, ${nums[i+2]}")
        }
        ++i
    }
    instructions
}

//println("Reading bytes...")
int[] bytes = new File('challenge.bin').bytes.collect { it & 0xff }
//println("Converting to numbers")
List<Integer> nums = readNums(bytes)
//println("Generating instructions")
List<Instr> instructions = readInstr(nums)
//println("Executing:")
Stack stack = new Stack()
int[] registers = [0, 0, 0, 0, 0, 0, 0, 0]
int pointer = 0
while (pointer < instructions.size()) {
    Instr instr = instructions[pointer]
    if (args.size() > 0 && args[0] == 'debug') println(instr)
    Integer newPointer = instr.process(stack, registers)
    if (newPointer != null) {
        if (newPointer == -1) {
            return
        }
        int realNewPointer = -1
        for (int i = 0; i < instructions.size(); ++i) {
            if (instructions[i].address == newPointer) {
                realNewPointer = i
            }
        }
        if (realNewPointer == -1) {
            throw new RuntimeException("Received new pointer")
        }
        pointer = realNewPointer
    } else {
        ++pointer
    }
}
