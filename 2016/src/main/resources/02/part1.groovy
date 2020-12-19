def main(String input) {
    new File(input).text
        .split('\n')
        .collect {
            it.collect { 
                Move.valueOf(it) }
            }
        .inject([]) { cur, next -> 
            cur.addAll(next)
            cur << Move.STOP
            cur
        }.inject(new Pos(x:1, y:1)) { cur, mv ->
            cur.move(mv)
        }
}

class Pos {
    int x
    int y
    static final int n = 3

    Pos move(Move move){
        if(move == Move.STOP){
            println toNum()
            return this
        }
        int newX = x + move.dx
        int newY = y + move.dy
        if (newX == n || newX == -1 || newY == n || newY == -1){
            return this
        }
        return new Pos(x:newX, y:newY)
    }
    def toNum(){
        y * n + x + 1 
    }
}


enum Move {
    L(-1, 0),
    R(1,0),
    U(0, -1),
    D(0, 1),
    STOP(0,0);

    int dx
    int dy

    Move(int dx, int dy){
        this.dx = dx
        this.dy = dy
    }
}


main('sample.txt')
println()
main('input.txt')
