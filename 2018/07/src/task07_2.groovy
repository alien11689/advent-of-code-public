import groovy.transform.ToString

def text = new File('input.txt').text.trim()
int workersAmount = 5

//def text = new File('input2.txt').text.trim()
//int workersAmount = 2

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

println(instructions)

@ToString
class Worker {
    static Map<Character, Integer> times = ('A'..'Z').collectEntries {
        [(it): ((it as char) as int) - 64 + 60]
    }
    static {
        println(times)
    }
    String cur
    Integer timeout
    int seconds = 0

    boolean isFinished() {
        timeout == seconds
    }

    def tick() {
        seconds++
    }

    boolean isEmpty() {
        !cur
    }

    def makeEmpty() {
        cur = null
        seconds = 0
        timeout = null
    }

    def assign(String letter) {
        cur = letter
        timeout = times[letter]
        seconds = 0
    }
}

List order = []

def workers = (1..workersAmount).collect { new Worker() }

int ticks = 0
while (true) {
    println(ticks)
    println(order)
    println(workers)
    workers.findAll { it.finished }.collect {
        String letter = it.cur
        order << letter
        it.makeEmpty()
        Instr toDelete = instructions.find { it.dest == letter }
        instructions -= toDelete
        instructions.findAll { it.before.contains(letter) }.each {
            it.before.remove(letter)
        }
    }
    if (instructions.empty && workers.every { Worker it -> it.empty }) {
        break
    }

    Worker nextWorker = workers.find { it.empty }
    boolean shouldTick = true
    if (nextWorker && instructions) {
        Instr available = instructions.findAll { it.before == [] as Set }.findAll {
            !workers.collect { it.cur }.findAll().contains(it.dest)
        }.sort { it.dest }.find()
        if (available) {
            String letter = available.dest
            nextWorker.assign(letter)
            shouldTick = false
        }
    }
    if (shouldTick) {
        workers.findAll { !it.empty }.each { it.tick() }
        ticks++
    }
}
println(ticks)
println(order.join())
