input = 'cqjxxyzz'

int maxLength = 8
int continuousSize = 3
forbidden = ['i', 'o', 'l']
letters = 'a'..'z'

def next(String input){
    boolean changed = false
    boolean upgradedZ = false
    input.reverse().inject('') { acc, cur -> 
        if (changed && !upgradedZ){
            acc + cur
        }else{
            if(cur == 'z'){
                acc += 'a'
                upgradedZ = true
            }else{
                acc += nextChar(cur)
                upgradedZ = false
            }
            changed = true
            acc
        }
    }.reverse()

}

def nextChar(ch){
    letters[letters.indexOf(ch) + (ch in ['h', 'n', 'k'] ?2:1)]
}

def simpleNextChar(ch){
    if(ch == 'z'){
        return '2'
    }
    return letters[letters.indexOf(ch) + 1]
}

def isValid(String input){
    boolean containsThree = false
    Set<String> doubleCh = [] as Set
    String first = input[0]
    String second = input[1]
    if(first == second){
       doubleCh << first 
    }
    for(third in input[2..-1]){
        if (second == third){
            doubleCh << second
        }
        if(simpleNextChar(first) == second && simpleNextChar(second) == third){
            containsThree = true
        }
        first = second
        second = third
    }
    println doubleCh
    println containsThree
    return containsThree && doubleCh.size() >= 2
}

iteration = next(input)
println "Next of dqbeansu is ${next('dqbeansu')}"

while(!isValid(iteration)){
    iteration = next(iteration)
    println iteration
    if(iteration == 'ghjaabcc'){
        println 'Example?'
        break
    }
}
