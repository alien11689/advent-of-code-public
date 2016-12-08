def main(String input, int n, int m) {
    new File(input).text
        .split('\n')
        .collect { Util.getCommand(it)}
        .inject(Util.generateBoard(n,m)) {board, command ->
            def newBoard = command.apply(board)
            Util.printlnBoard(board)
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
        def board = Util.generateBoard(old[0].size(), old.size())

        board
    }
}
@groovy.transform.Immutable
class RotateColumn {
    int y
    int by
    def apply(List<List<String>> old){
        def board = Util.generateBoard(old[0].size(), old.size())

        board
    }
}
@groovy.transform.Immutable
class RotateRow {
    int x
    int by
    def apply(List<List<String>> old){
        def board = Util.generateBoard(old[0].size(), old.size())

        board
    }
}

class Util {
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
//println(main('input.txt', 50, 6))
