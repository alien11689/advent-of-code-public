//String input = new File('day13.sample').text
String input = new File('day13.data').text

@groovy.transform.ToString
class Rule {
    Set<String> names
    int score
}

rules = input.split('\n').collect {
    String [] p = it.split(' ')
    [p[0], p[2], p[3], p[10][0..-2]]
    new Rule(names: [p[0], p[10][0..-2]] as Set, score: p[2] == 'gain' ? (p[3] as int): (-1 * (p[3] as int)))
}.inject([:]){ acc, cur -> 
    if(cur.names in acc){
        acc[cur.names] += cur.score
        acc
    }else{
        acc[cur.names] = cur.score
        acc
    }
}

names = rules.keySet().collectMany { it } as Set

println rules
println names

def calcHappiness(List<String> names){
    String first = names[0]
    String cur = first
    int sum = 0
    for(String it in names[1..-1] ){
       sum += rules[[cur, it] as Set]
       cur = it
    }
    sum += rules[[cur, first] as Set]
    return sum
}

// println calcHappiness(['David', 'Alice', 'Bob', 'Carol'])

def swap(List<String> l, int a, int b){
    List<String> k = l.collect()
    String s = k[a]
    k[a] = k[b]
    k[b] = s
    k
}

List<String> best = names as List
int bestHap = calcHappiness(best)

def memory = [] as Set
def random = new Random()

(1..10000).each {
    println it
    int from = random.nextInt() % best.size()
    int to = random.nextInt() % best.size()
    println "$from, $to"
    def newList = swap(best, from, to)
    println newList
    if(!(newList in memory)){
        memory << newList
    
        int newHap = calcHappiness(newList)
        if(newHap > bestHap){
            best = newList
            bestHap = newHap
        }
    }
    
    println "Iteration $it: $best => $bestHap"

}
