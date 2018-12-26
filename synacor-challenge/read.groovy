List<Integer> readMemory(int[] bytes) {
    List<Integer> memory = []
    for (int i = 0; i < bytes.size(); i += 2) {
        int a = bytes[i]
        int b = bytes[i + 1]
        memory << a + b * (2**8)
    }
    return memory
}

private static int valueOrRegisterValue(int num, int[] registers) {
    num >= 32768 ? registers[num % 32768] : num
}

debug = false

int process(Stack<Integer> stack, int[] registers, List<Integer> memory, int pointer) {
    int curInstr = memory[pointer]
    switch (curInstr) {
        case Opcode.HALT:
            if (debug) println("$pointer\t\t$registers\t\tHALT")
            return -1
        case Opcode.NOOP:
            if (debug) println('NOOP')
            return pointer + 1
        case Opcode.OUT:
            int a = memory[pointer + 1]
            print(valueOrRegisterValue(a, registers) as char)
            return pointer + 2
        case Opcode.JMP:
            int a = memory[pointer + 1]
            if (debug) println("$pointer\t\t$registers\t\tJMP $a")
            return valueOrRegisterValue(a, registers)
        case Opcode.JT:
            int a = memory[pointer + 1]
            int b = memory[pointer + 2]
            if (debug) println("$pointer\t\t$registers\t\tJT $a $b")
            if (valueOrRegisterValue(a, registers) > 0) {
                return valueOrRegisterValue(b, registers)
            }
            return pointer + 3
        case Opcode.JF:
            int a = memory[pointer + 1]
            int b = memory[pointer + 2]
            if (debug) println("$pointer\t\t$registers\t\tJF $a $b")
            if (valueOrRegisterValue(a, registers) == 0) {
                return valueOrRegisterValue(b, registers)
            }
            return pointer + 3
        case Opcode.SET:
            //println("SET $a $b")
            int a = memory[pointer + 1]
            int b = memory[pointer + 2]
            if (debug) println("$pointer\t\t$registers\t\tSET $a $b")
            registers[a % 32768] = valueOrRegisterValue(b, registers)
            return pointer + 3
        case Opcode.ADD:
            int a = memory[pointer + 1]
            int b = memory[pointer + 2]
            int c = memory[pointer + 3]
            if (debug) println("$pointer\t\t$registers\t\tADD $a $b $c")
            registers[a % 32768] = (valueOrRegisterValue(b, registers) + valueOrRegisterValue(c, registers)) % 32768
            return pointer + 4
        case Opcode.MULT:
            int a = memory[pointer + 1]
            int b = memory[pointer + 2]
            int c = memory[pointer + 3]
            if (debug) println("$pointer\t\t$registers\t\tMULT $a $b $c")
            registers[a % 32768] = (valueOrRegisterValue(b, registers) * valueOrRegisterValue(c, registers)) % 32768
            return pointer + 4
        case Opcode.MOD:
            int a = memory[pointer + 1]
            int b = memory[pointer + 2]
            int c = memory[pointer + 3]
            if (debug) println("$pointer\t\t$registers\t\tMOD $a $b $c")
            registers[a % 32768] = (valueOrRegisterValue(b, registers) % valueOrRegisterValue(c, registers)) % 32768
            return pointer + 4
        case Opcode.EQ:
            int a = memory[pointer + 1]
            int b = memory[pointer + 2]
            int c = memory[pointer + 3]
            if (debug) println("$pointer\t\t$registers\t\tEQ $a $b $c")
            registers[a % 32768] = valueOrRegisterValue(b, registers) == valueOrRegisterValue(c, registers) ? 1 : 0
            return pointer + 4
        case Opcode.PUSH:
            int a = memory[pointer + 1]
            if (debug) println("$pointer\t\t$registers\t\tPUSH $a")
            stack.push(valueOrRegisterValue(a, registers))
            return pointer + 2
        case Opcode.POP:
            int a = memory[pointer + 1]
            if (debug) println("$pointer\t\t$registers\t\tPOP $a")
            registers[a % 32768] = stack.pop()
            return pointer + 2
        case Opcode.GT:
            int a = memory[pointer + 1]
            int b = memory[pointer + 2]
            int c = memory[pointer + 3]
            if (debug) println("$pointer\t\t$registers\t\tGT $a $b $c")
            registers[a % 32768] = valueOrRegisterValue(b, registers) > valueOrRegisterValue(c, registers) ? 1 : 0
            return pointer + 4
        case Opcode.AND:
            int a = memory[pointer + 1]
            int b = memory[pointer + 2]
            int c = memory[pointer + 3]
            if (debug) println("$pointer\t\t$registers\t\tAND $a $b $c")
            registers[a % 32768] = (valueOrRegisterValue(b, registers) & valueOrRegisterValue(c, registers)) % 32768
            return pointer + 4
        case Opcode.OR:
            int a = memory[pointer + 1]
            int b = memory[pointer + 2]
            int c = memory[pointer + 3]
            if (debug) println("$pointer\t\t$registers\t\tOR $a $b $c")
            registers[a % 32768] = (valueOrRegisterValue(b, registers) | valueOrRegisterValue(c, registers)) % 32768
            return pointer + 4
        case Opcode.NOT:
            int a = memory[pointer + 1]
            int b = memory[pointer + 2]
            if (debug) println("$pointer\t\t$registers\t\tNOT $a $b")
            registers[a % 32768] = Integer.parseInt(String.format("%15s", Integer.toBinaryString(valueOrRegisterValue(b, registers)))
                    .replace(' ', '0')
                    .collect {
                it == '0' ? '1' : '0'
            }.join(), 2) % 32768
            return pointer + 3
        case Opcode.CALL:
            int a = memory[pointer + 1]
            if (debug) println("$pointer\t\t$registers\t\tCALL $a")
            stack.push(pointer + 2)
            return valueOrRegisterValue(a, registers)
        case Opcode.RMEM:
            int a = memory[pointer + 1]
            int b = memory[pointer + 2]
            if (debug) println("$pointer\t\t$registers\t\tRMEM $a $b")
            registers[a % 32768] = memory[valueOrRegisterValue(b, registers)] % 32768
            return pointer + 3
        case Opcode.WMEM:
            int a = memory[pointer + 1]
            int b = memory[pointer + 2]
            if (debug) println("$pointer\t\t$registers\t\tWMEM $a $b")
            memory[valueOrRegisterValue(a, registers)] = valueOrRegisterValue(b, registers)
            return pointer + 3
        case Opcode.RET:
            if (debug) println("$pointer\t\t$registers\t\tRET")
            return stack.pop()
        case Opcode.IN:
            int a = memory[pointer + 1]
//            println("Saving output in $a")
            int value = System.in.read() % 32768
            if (value == 48) {
                debug = true
                println "Turn on debug"
                value = System.in.read() % 32768
            }
//            println("Read value $value")
            registers[a % 32768] = value
            return pointer + 2
        default:
            throw new RuntimeException("Unknown instr $curInstr")
    }
}

class Opcode {
    static int HALT = 0
    static int SET = 1
    static int PUSH = 2
    static int POP = 3
    static int EQ = 4
    static int GT = 5
    static int JMP = 6
    static int JT = 7
    static int JF = 8
    static int ADD = 9
    static int MULT = 10
    static int MOD = 11
    static int AND = 12
    static int OR = 13
    static int NOT = 14
    static int RMEM = 15
    static int WMEM = 16
    static int CALL = 17
    static int RET = 18
    static int OUT = 19
    static int IN = 20
    static int NOOP = 21

}

//println("Reading bytes...")
int[] bytes = new File('challenge.bin').bytes.collect { it & 0xff }
//println("Converting to numbers")
List<Integer> memory = readMemory(bytes)
//println("Executing:")
Stack stack = new Stack()
int[] registers = [0, 0, 0, 0, 0, 0, 0, 0]
int pointer = 0
while (pointer < memory.size()) {
//    if (args.size() > 0 && args[0] == 'debug') println(instr)
    int newPointer = process(stack, registers, memory, pointer)
    if (newPointer == -1) {
        return
    }
    pointer = newPointer
}
