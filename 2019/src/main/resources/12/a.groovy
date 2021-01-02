def xxs = [
    [-1,7,3],
    [12,2,-13],
    [14,18,-8],
    [17,4,-4]
]

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

1000.times {
    println it
    //for(int i =0; i < xxs.size(); ++i){ println(xxs[i] + '->'+vvs[i])}
    for (int i = 0; i < xxs.size(); ++i){
        def curX = xxs[i]
        def curV = vvs[i]
        vvs[i] = [curV[0] + xxs.collect {signum(curX[0],it[0])}.sum(), curV[1] + xxs.collect {signum(curX[1],it[1])}.sum(), curV[2] + xxs.collect {signum(curX[2],it[2])}.sum()]
    }
    for (int i = 0; i < xxs.size(); ++i){
        def curX = xxs[i]
        xxs[i] = [curX[0] + vvs[i][0], curX[1] + vvs[i][1],curX[2] + vvs[i][2],]
    }
}

int sum = 0
for (int i = 0; i< xxs.size(); ++i){
    sum += xxs[i].collect {it.abs()}.sum() * vvs[i].collect {it.abs()}.sum()
}

println sum
