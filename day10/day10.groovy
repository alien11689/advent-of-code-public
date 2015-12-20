import groovy.transform.*

String input = '1321131112'

@ToString
class Pair{
    String ch
    int count
    def val(){
        "$count$ch"
    }
}

def res = input
(1..40).each{
    res = res.inject ([]) {
        acc, cur -> if(acc && acc.last().ch == cur){
            ++acc.last().count
            //println acc
            acc
        }else {
            acc << new Pair(ch: cur, count:1)
            //println acc
            acc
        }
    }.inject('') {
        acc, cur-> acc + cur.val()
    }
    println "$it -> ${res.size()}"
}
