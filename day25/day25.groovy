column = 3083
row = 2978
bound = 10000

board = (1..bound).collect {
    (1..bound).collect {
        0L
    }
}

board[0][0] = 20151125L

def next(cur){
    cur * 252533 % 33554393
}

i = 0 //row
j = 0 //collumn
cur = board[0][0]
iter = 1

def nextIdx(int i, int j){
    if(i == 0 && j == 0){
        ++i
    }else if(j == bound - 1){
        def tmp = i
        i = j
        j = tmp + 1
        println ([i,j])
    }else if (i == 0){
        i = j + 1
        j = 0
        println ([i,j])
    }else {
        ++j
        --i
    }
    return [i,j]
}

while(board[row -1][column -1] == 0l ){
    if(i == 0 && j == 0){
        board[0][0] = 20151125L
    }else{
        board[i][j] = next(cur)
    }
    cur = board[i][j]
    (i,j) = nextIdx(i,j)
    ++iter
}

//board.each {
//    println it
//}
println (board[row -1][column -1])
