def main(String input, int n, int m) {
    new File(input).text
        .split('\n')
        .collect { Util.getCommand(it)}
        .inject(Util.generateBoard(n,m)) {board, command ->
            println command
            def newBoard = command.apply(board)
            Util.printlnBoard(newBoard)
            newBoard
        }
        .flatten()
        .collect {it == '#' ? 1 : 0}
        .sum()
        
}

@groovy.transform.Immutable
class Rect {
    int a
    int b

    def apply(List<List<String>> old){
        def board = Util.copyBoard(old)
        for(int i = 0; i < a; ++i){
            for(int j = 0; j < b; ++j){
                board[j][i] = '#'
            }
        }
        board
    }
}
@groovy.transform.Immutable
class RotateColumn {
    int x
    int by
    def apply(List<List<String>> old){
        def board = Util.copyBoard(old)
        def size = board.size()
        for(int i = 0; i < size; ++i){
            board[(i + by)%size][x] = old[i][x]
        }
        board
    }
}
@groovy.transform.Immutable
class RotateRow {
    int y
    int by
    def apply(List<List<String>> old){
        def board = Util.copyBoard(old)
        def size = board[0].size()
        for(int i = 0; i < size; ++i){
            board[y][(i + by)%size] = old[y][i]
        }
        board
    }
}

class Util {

static def copyBoard(def board){
    def newBoard = generateBoard(board[0].size(), board.size())
    board.eachWithIndex { line, j ->
        line.eachWithIndex {cell, i ->
            newBoard[j][i] = cell 
        }
    }
    newBoard
}

static def getCommand(String line){
    def split = line.split(/ /)
    if(line.startsWith('rect')){
        def axb = split[1].split(/x/)
        new Rect(axb[0] as int, axb[1] as int)
    }else if(line.startsWith('rotate column')){
        new RotateColumn(split[2].split(/=/)[1] as int, split[4] as int)
    }else if(line.startsWith('rotate row')){
        new RotateRow(split[2].split(/=/)[1] as int, split[4] as int)
    }
}

static def printlnBoard(board){
    board.each {
        println it.join('')
    }
    println()
}

static def generateBoard(int n, int m){
    (1..m).collect {
        (1..n).collect { '.' }
    }
}
}

println(main('sample.txt', 7, 3))
println()
println(main('input.txt', 50, 6))
