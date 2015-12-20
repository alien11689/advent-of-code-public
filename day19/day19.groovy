String[] input = new File('day19.data').text.split('\n').findAll {it.size() > 0}

String initial = input[-1]

@groovy.transform.ToString
class Repl{
    String from
    String to
    def size(){
        return from.size()
    }
}

def replacements = input[0..<(-1)].collect {
    String [] p = it.split(' ')
    new Repl(from: p[0], to: p[2])
}

println replacements
println initial

def molecules = [] as Set

replacements.each {repl -> 
    for(i in (0..<(initial.size() - repl.size()))){
        int point = initial.indexOf(repl.from, i)
        if(point < 0){
            break
        }
        molecules << initial[0..<i] + initial[i..-1].replaceFirst(repl.from, repl.to)
    }
}

println molecules.size()
