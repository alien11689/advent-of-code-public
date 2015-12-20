String[] input = new File('day19.data').text.split('\n').findAll {it.size() > 0}

String destination = input[-1]
String initial = 'e'

@groovy.transform.ToString
class Repl{
    String from
    String to
    def size(){
        return from.size()
    }
}

replacements = input[0..<(-1)].collect {
    String [] p = it.split(' ')
    new Repl(from: p[2], to: p[0])
}

println replacements

def it = 0
cur = destination
int counter = 0
while(cur != initial){
    println "Iteration $it"
    ++it
    tmp = cur
    
    for(x in replacements){
        if(cur.contains(x.from)){
            ++counter
            cur = cur.replaceFirst(x.from, x.to)
        }
    }

    if (tmp == cur){
        cur = destination
        counter = 0
        Collections.shuffle(replacements)
    }

}
println counter 
