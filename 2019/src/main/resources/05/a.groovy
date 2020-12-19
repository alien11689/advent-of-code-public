List<Long> v = new File('input.txt').text.split(',').collect { it as long} as ArrayList

int pos = 0
//v[1] = 12
//v[2] = 2

while(true){
    int op = v[pos]
    if(op == 99L){
        println v[0]
        return
    }
    if(op == 1){
        v[v[pos + 3]] = v[v[pos + 1]] + v[v[pos + 2]]
        pos+=4
    }else if(op == 1101){
        v[v[pos + 3]] = v[pos + 1] + v[pos + 2]
        pos+=4
    }else if(op == 101){
        v[v[pos + 3]] = v[pos + 1] + v[v[pos + 2]]
        pos+=4
    }else if(op == 1001){
        v[v[pos + 3]] = v[v[pos + 1]] + v[pos + 2]
        pos+=4
    }else if(op == 2) {
        v[v[pos + 3]] = v[v[pos + 1]] * v[v[pos + 2]]
        pos += 4
    }else if(op == 1002) {
        v[v[pos + 3]] = v[v[pos + 1]] * v[pos + 2]
        pos += 4
    }else if(op == 1102) {
        v[v[pos + 3]] = v[pos + 1] * v[pos + 2]
        pos += 4
    }else if(op == 102) {
        v[v[pos + 3]] = v[pos + 1] * v[v[pos + 2]]
        pos += 4
    }else if(op == 3){
        v[v[pos + 1]] = 1
        pos+= 2
    }else if(op == 104){
        println v[pos + 1]
        pos += 2
    }else if(op == 4){
        println v[v[pos + 1]]
        pos += 2
    } else {
        println "Unknown $op"
        return
    }
}
