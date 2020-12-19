
String[] rules = new File('day7_2.data').text.split('\n')

class And {
    String s1
    String s2
    String r
    boolean result(Map<String,Integer> wires){
        def v1 = s1.matches(/^\d.*/)? (s1 as Integer) : wires[s1]
        def v2 = s2.matches(/^\d.*/)? (s2 as Integer) : wires[s2]
        if (v1 != null && v2 != null){
            wires[r] = v1 & v2
            return true
        }
        return false
    }
}

class Or {
    String s1
    String s2
    String r
    boolean result(Map<String,Integer> wires){
        def v1 = s1.matches(/^\d.*/)? (s1 as Integer) : wires[s1]
        def v2 = s2.matches(/^\d.*/)? (s2 as Integer) : wires[s2]
        if (v1 != null && v2 != null){
            wires[r] = v1 | v2
            return true
        }
        return false
    }
}

class LShift {
    String s1
    String r
    int shift
    boolean result(Map<String,Integer> wires){
        def v1 = s1.matches(/^\d.*/)? (s1 as Integer) : wires[s1]
        if (v1 != null){
            wires[r] = v1 << shift
            return true
        }
        return false
    }
}

class RShift {
    String s1
    String r
    int shift
    boolean result(Map<String,Integer> wires){
        def v1 = s1.matches(/^\d.*/)? (s1 as Integer) : wires[s1]
        if (v1 != null){
            wires[r] = v1 >> shift
            return true
        }
        return false
    }
}

class Not {
    String s1
    String r
    boolean result(Map<String,Integer> wires){
        def v1 = s1.matches(/^\d.*/)? (s1 as Integer) : wires[s1]
        if (v1 != null){
            wires[r] = v1 ^ 65535
            return true
        }
        return false
    }
}

class Ident {
    String s1
    String r
    boolean result(Map<String,Integer> wires){
        if (s1 in wires){
            wires[r] = wires[s1]
            return true
        }
        return false
    }
}

Map<String,Integer> wires = [:]
def gates = [] 

rules.each {rule->
    String [] s = rule.split(' ')
    if(s[1] == 'AND'){
        gates << new And(s1: s[0], s2: s[2], r: s[4]) 
    } else if(s[1] == 'OR'){
        gates << new Or(s1: s[0], s2: s[2], r: s[4]) 
    }else if(s[0] == 'NOT'){
        gates << new Not(s1: s[1], r: s[3]) 
    }else if(s[1] == 'LSHIFT'){
        gates << new LShift(s1: s[0], shift: s[2] as int, r: s[4]) 
    }else if(s[1] == 'RSHIFT'){
        gates << new RShift(s1: s[0], shift: s[2] as int, r: s[4]) 
    }else if(s[0].matches(/^\d.*/)){
        wires[s[2]] = s[0] as int
    }else {
        gates << new Ident(s1: s[0], r: s[2]) 
    }
}

println gates
println wires

while(!gates.empty){
    def toDelete = []
    for(g in gates){
        if(g.result(wires)){
            toDelete << g
        }
    }

    if(toDelete.empty){
        break
    }
    toDelete.each {
        gates -= it
    }
    println toDelete
    println "Gates size is ${gates.size()}"
    println "Wires size is ${wires.size()}"
}

println gates
println wires

println wires.a
