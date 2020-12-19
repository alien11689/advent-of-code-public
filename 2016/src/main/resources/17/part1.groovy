import java.security.MessageDigest

def main(String input) {
    Pos pos = new Pos(path: input)
    Queue q = new LinkedList()
    q << pos
    while(true){
        Pos cur = q.poll()
        println cur
        if(!cur) {
            return "No escape"
        }

        if(cur.x == 3 && cur.y == 3){
            return cur.path
        }
        def available = md5(cur.path)[0..3].collect {
            it in ['b', 'c', 'd', 'e', 'f'] ? 1 : 0
        }
        
        def moves = [
            available[0] ? 'U' as char: null,
            available[1] ? 'D' as char : null,
            available[2] ? 'L' as char : null,
            available[3] ? 'R' as char : null,
        ].findAll()

        moves.collect {newPos(cur, it)}.findAll {it.valid()}.each {
            q << it
        }
    }
}

def newPos(Pos pos, char x){
    new Pos(
        x: pos.x + (x == 'L' ? -1 : (x == 'R' ? 1 : 0)),
        y: pos.y + (x == 'U' ? -1 : (x == 'D' ? 1 : 0)),
        path: pos.path + x,
        size: pos.size + 1 
    )
}

@groovy.transform.ToString
class Pos {
    int x = 0
    int y = 0
    String path = ""
    int size = 0

    def valid(){
        x in (0..3) && y in (0..3)
    }
}

def md5(String s) {
        MessageDigest digest = MessageDigest.getInstance("MD5")
        digest.update(s.bytes);
        new BigInteger(1, digest.digest()).toString(16).padLeft(32, '0')
}

println(main('hijkl'))
println()
println(main('ihgpwlah'))
println()
println(main('kglvqrro'))
println()
println(main('ulqzkmiv'))
println()

println(main('mmsxrhfx'))
