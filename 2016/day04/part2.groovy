def main(String input) {
    new File(input).text
        .split('\n')
        .collect {
            new Room(it)
        }
        .findAll {it.valid()}
        .find {
            it.text.contains('north')        
        }
}

@groovy.transform.Immutable
class Pair implements Comparable<Pair>{
    int amount
    char c

    int compareTo(Pair o){
        int a = o.amount - amount
        if(a == 0){
          return c.compareTo(o.c)  
        }
        return a
    }
}


@groovy.transform.ToString
class Room {
    int sectorId
    List<Character> checksum
    List<Pair> encode
    String text

    Room (String roomEncoded){
        def split = roomEncoded.split('-')
        encode = split[0..<-1].join('')
            .collect {it}
            .sort()
            .inject ([:]) { old, c ->
                if(old[c]){
                    old[c] = old[c] + 1
                }else{
                    old[c] = 1
                }
                old
            }.inject([:]){ cur, e ->
                def l = cur[e.value] ?: []
                cur[e.value] = l + e.key
                cur
            }.collectMany { e ->
                e.value.collect {
                    new Pair(e.key, it as char)
                }
            }.sort()
        def check = split[-1].split(/[\[\]]/)
        sectorId = check[0] as int
        checksum = check[1].collect {it as char}
        text = split[0..<-1].join(' ').collect {
            it == ' ' ? it : rot(it as char, sectorId)
        }.join('')
    }

    def valid(){
        encode.take(5).collect {it.c} == checksum
    }
    def rot(char c, int amount){
        int v = (c as int) - 97
        return (97 + (v+amount)%26) as char
    }
}

println(main('sample.txt'))
println()
println(main('input.txt'))
