def xxs = [
    [-1,7,3],
    [12,2,-13],
    [14,18,-8],
    [17,4,-4]
]
def goal = [
    [-1,7,3],
    [12,2,-13],
    [14,18,-8],
    [17,4,-4]
]
/**xxs = [
    [-1,0,2],
    [2,-10,-7],
    [4,-8,8],
    [3,5,-1]
]
goal = [
    [-1,0,2],
    [2,-10,-7],
    [4,-8,8],
    [3,5,-1]
]
*/
/*xxs = [
    [-8,-10,0],
    [5,5,10],
    [2,-7,3],
    [9,-8,-3],
]
*/
def vvs = [
    [0,0,0],
    [0,0,0],
    [0,0,0],
    [0,0,0],
]

int signum(a,b){
    if(a == b){
        return 0
    }
    return a < b ? 1 : -1    
}

long iter = 0
long nextX = 0
long nextY = 0
long nextZ = 0
while(true) {
    ++iter
    for (int i = 0; i < xxs.size(); ++i){
        def curX = xxs[i]
        def curV = vvs[i]
        vvs[i] = [curV[0] + xxs.collect {signum(curX[0],it[0])}.sum(), curV[1] + xxs.collect {signum(curX[1],it[1])}.sum(), curV[2] + xxs.collect {signum(curX[2],it[2])}.sum()]
    }
    for (int i = 0; i < xxs.size(); ++i){
        def curX = xxs[i]
        xxs[i] = [curX[0] + vvs[i][0], curX[1] + vvs[i][1],curX[2] + vvs[i][2],]
    }
    if(nextX == 0 && vvs.collect {it[0]}.every {it == 0}){
        println "$iter -> x"
        nextX = iter
    }
    if(nextY == 0 && vvs.collect {it[1]}.every {it == 0}){
        println "$iter -> y"
        nextY = iter
    }
    if(nextZ == 0 && vvs.collect {it[2]}.every {it == 0}){
        println "$iter -> z"
        nextZ = iter
    }
    if(nextX > 0 && nextY >0 && nextZ > 0){
        println("$nextX, $nextY, $nextZ")
        println("il: " + (nextX * nextY * nextZ))
        break
    }
}

