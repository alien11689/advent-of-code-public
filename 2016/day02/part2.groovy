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
        }.inject(new Pos(x:0, y:2)) { cur, mv ->
            cur.move(mv)
        }
}

class Pos {
    int x
    int y
    static final int n = 5

    static final def table = [
        [null, null, '1', null, null],
        [null, '2', '3', '4', null],
        ['5', '6', '7', '8', '9'],
        [null, 'A', 'B', 'C', null],
        [null, null, 'D', null, null],
    ]

    Pos move(Move move){
        if(move == Move.STOP){
            println toNum()
            return this
        }
        int newX = x + move.dx
        int newY = y + move.dy
        if (newX == n || newX == -1 || newY == n || newY == -1 || !table[newY][newX]){
            return this
        }
        return new Pos(x:newX, y:newY)
    }
    def toNum(){
        table[y][x]
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
