import groovy.transform.ToString

def text = new File('input.txt').text.trim()

def lines = text.split('\n')

@ToString
class Instr {
    String dest
    TreeSet<String> before = [] as Set
}

def instructions = (lines.collect { it.split(' ')[7] } as Set).collect { new Instr(dest: it) }

lines.each { l ->
    String[] split = l.split(' ')
    String dest = split[7]
    String from = split[1]
    Instr instr = instructions.find { it.dest == dest }
    instr.before << from
}



(instructions.before.flatten() as Set).each { letter ->
    if (instructions.find { it.dest == letter }) {
        //nothing
    } else {
        instructions << new Instr(dest: letter)
    }
}

//println(instructions)

List order = []

while (!instructions.empty) {
//    println(order)
    Instr next = instructions.findAll { it.before == [] as Set }.sort { it.dest }.first()
    String letter = next.dest
    order << letter
    instructions -= next
    instructions.findAll { it.before.contains(letter) }.each {
        it.before.remove(letter)
    }
}

println(order.join())