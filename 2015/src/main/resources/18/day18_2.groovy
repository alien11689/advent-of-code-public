List<List<String>> initial = new File('day18.data').text.split('\n').collect {it.split('')}

size = initial.size()

def neighbours(board, int i, int j){
    [
        [i, j - 1],
        [i, j + 1],
        [i - 1, j - 1],
        [i - 1, j],
        [i - 1, j + 1],
        [i + 1, j - 1],
        [i + 1, j],
        [i + 1, j + 1],
    ].findAll {
        (0..<size).contains (it[0]) && (0..<size).contains (it[1])
    }
}

def getNextLight(board, int i, int j){
    def n = neighbours(board, i, j)
    String cur = board[i][j]
    def lights = n.collect {
        board[it[0]][it[1]]
    }.findAll{
        it == '#'
    }.sum{1}
    if(cur == '#'){
        if(lights == 2 || lights == 3){
            '#'
        }else{
            '.'
        }
    }else{
        lights == 3 ? '#' : '.'
    }
}

def lightOnCorners(board){
    board[0][0] = '#' 
    board[0][size -1] = '#' 
    board[size-1][0] = '#' 
    board[size-1][size-1] = '#' 
}

board = initial
lightOnCorners(board)
//println initial
(1..100).each{
    def newBoard = initial.collect { it.collect {it} } 
    (0..<size).each{ i->
        (0..<size).each{ j-> 
           newBoard[i][j]  = getNextLight(board, i, j)
        }
    }
    lightOnCorners(newBoard)
    board = newBoard
    println "Iteration $it"
  //  println board
}

println board.collectMany {it}.findAll{it == '#'}.sum{1} 
