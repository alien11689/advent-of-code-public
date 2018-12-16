import groovy.transform.Canonical

static String readInput(String file) {
    return new File(file).text
}

static List<String> readInputAsLines(String file) {
    return readInput(file).trim().split('\n')
}

List<String> lines = readInputAsLines('input.txt')

interface Operation {
    int[] apply(int A, int B, int C, int[] registers)
}

@Canonical
class Addr implements Operation {
    @Override
    int[] apply(int A, int B, int C, int[] registers) {
        int[] result = registers.collect()
        result[C] = registers[A] + registers[B]
        return result
    }
}

@Canonical
class Addi implements Operation {
    @Override
    int[] apply(int A, int B, int C, int[] registers) {
        int[] result = registers.collect()
        result[C] = registers[A] + B
        return result
    }
}

@Canonical
class Mulr implements Operation {
    @Override
    int[] apply(int A, int B, int C, int[] registers) {
        int[] result = registers.collect()
        result[C] = registers[A] * registers[B]
        return result
    }
}

@Canonical
class Muli implements Operation {
    @Override
    int[] apply(int A, int B, int C, int[] registers) {
        int[] result = registers.collect()
        result[C] = registers[A] * B
        return result
    }
}

@Canonical
class Banr implements Operation {
    @Override
    int[] apply(int A, int B, int C, int[] registers) {
        int[] result = registers.collect()
        result[C] = registers[A] & registers[B]
        return result
    }
}

@Canonical
class Bani implements Operation {
    @Override
    int[] apply(int A, int B, int C, int[] registers) {
        int[] result = registers.collect()
        result[C] = registers[A] & B
        return result
    }
}

@Canonical
class Borr implements Operation {
    @Override
    int[] apply(int A, int B, int C, int[] registers) {
        int[] result = registers.collect()
        result[C] = registers[A] | registers[B]
        return result
    }
}

@Canonical
class Bori implements Operation {
    @Override
    int[] apply(int A, int B, int C, int[] registers) {
        int[] result = registers.collect()
        result[C] = registers[A] | B
        return result
    }
}

@Canonical
class Setr implements Operation {
    @Override
    int[] apply(int A, int B, int C, int[] registers) {
        int[] result = registers.collect()
        result[C] = registers[A]
        return result
    }
}

@Canonical
class Seti implements Operation {
    @Override
    int[] apply(int A, int B, int C, int[] registers) {
        int[] result = registers.collect()
        result[C] = A
        return result
    }
}

@Canonical
class Gtir implements Operation {
    @Override
    int[] apply(int A, int B, int C, int[] registers) {
        int[] result = registers.collect()
        result[C] = A > registers[B] ? 1 : 0
        return result
    }
}

@Canonical
class Gtri implements Operation {
    @Override
    int[] apply(int A, int B, int C, int[] registers) {
        int[] result = registers.collect()
        result[C] = registers[A] > B ? 1 : 0
        return result
    }
}

@Canonical
class Gtrr implements Operation {
    @Override
    int[] apply(int A, int B, int C, int[] registers) {
        int[] result = registers.collect()
        result[C] = registers[A] > registers[B] ? 1 : 0
        return result
    }
}

@Canonical
class Eqir implements Operation {
    @Override
    int[] apply(int A, int B, int C, int[] registers) {
        int[] result = registers.collect()
        result[C] = A == registers[B] ? 1 : 0
        return result
    }
}

@Canonical
class Eqri implements Operation {
    @Override
    int[] apply(int A, int B, int C, int[] registers) {
        int[] result = registers.collect()
        result[C] = registers[A] == B ? 1 : 0
        return result
    }
}

@Canonical
class Eqrr implements Operation {
    @Override
    int[] apply(int A, int B, int C, int[] registers) {
        int[] result = registers.collect()
        result[C] = registers[A] == registers[B] ? 1 : 0
        return result
    }
}

@Canonical
class UnknownOperation {
    int[] before
    int[] operands
    int[] after
    List<Operation> matches
    Operation operation = null
}

def readOperations(List<String> lines) {
    int i = 0
    List<Operation> availableOperations = [
            new Addi(),
            new Addr(),
            new Mulr(),
            new Muli(),
            new Setr(),
            new Seti(),
            new Banr(),
            new Bani(),
            new Borr(),
            new Bori(),
            new Gtir(),
            new Gtri(),
            new Gtrr(),
            new Eqir(),
            new Eqri(),
            new Eqrr(),
    ]
    int countWhenMultipleMatch = 0
    List<UnknownOperation> unknownOperations = []
    while (i < lines.size()) {
        if (!lines[i].startsWith('Before')) {
            break
        }
        String[] before = lines[i].replaceAll('\\[', '').replaceAll('\\]', '').replaceAll(',', '').split(/ /).findAll()
        int[] registers = [before[1] as int, before[2] as int, before[3] as int, before[4] as int]

        int[] operands = lines[i + 1].split(/ /).findAll().collect { it as int }

        String[] after = lines[i + 2].replaceAll('\\[', '').replaceAll('\\]', '').replaceAll(',', '').split(/ /).findAll()
        int[] registersAfter = [after[1] as int, after[2] as int, after[3] as int, after[4] as int]

        List<Operation> matches = availableOperations.findAll {
            it.apply(operands[1], operands[2], operands[3], registers) == registersAfter
        }
//
//        if (matches.size() == 1) {
//            mapping[operands[0]] = matches[0]
//        }

        unknownOperations << new UnknownOperation(registers, operands, registersAfter, matches)

        countWhenMultipleMatch += matches.size() >= 3 ? 1 : 0

        i += 4
    }
    //TODO read program
    println("Part 1: $countWhenMultipleMatch")
    println('--------------')
    Map<Integer, Operation> mapping = [:]
    unknownOperations.removeAll {it.matches.empty}
    while (!unknownOperations.empty && mapping.size() != 16) {
//        println("Unknown operastions size: ${unknownOperations.size()}")
//        println("Mapping size: ${mapping.size()}")
        Set<Operation> operandsToRemove = []
        unknownOperations.findAll { it.matches.size() == 1 }.each {
//            println("Found uniq $it.matches for ${it.operands[0]}")
            mapping[it.operands[0]] = it.matches[0]
            operandsToRemove << it.matches[0]
        }
        operandsToRemove.each { operand ->
            unknownOperations.each {
                it.matches.remove(operand)
            }
        }
        unknownOperations.removeAll { it.matches.empty }
    }
//    unknownOperations.each {println it}
//    println(mapping)

    int[] registers = (0..3).collect { 0 } as int[]
    while (i < lines.size()) {
        String cur = lines[i]
        if (cur.empty) {
            ++i
            continue
        }
        int[] operands = cur.split(/ /).findAll().collect { it as int }
        registers = mapping[operands[0]].apply(operands[1], operands[2], operands[3], registers)
        ++i
    }
    println("Part 2: ${registers[0]}")
}

readOperations(lines)
