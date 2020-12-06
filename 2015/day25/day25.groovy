column = 3083
row = 2978


def next(cur){
    cur * 252533 % 33554393
}

i = 1 //row
j = 1 //collumn
cur = 20151125L

def nextIdx(int i, int j){
    if(i == 1 && j == 1){
        ++i
    }else if (i == 1){
        i = j + 1
        j = 1
    }else {
        ++j
        --i
    }
    return [i,j]
}

while(i != row || j != column ){
    cur = next(cur)
    (i,j) = nextIdx(i,j)
}

println (cur)
