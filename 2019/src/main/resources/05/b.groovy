List<Long> v = new File('input.txt').text.split(',').collect { it as long} as ArrayList

int pos = 0

while(true){
    int op = v[pos]
    if(op == 99L){
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
    }else if(op == 1105) {
        if(v[pos + 1] != 0){
            pos = v[pos + 2]
        }else {
            pos += 3
        }
    }else if(op == 105) {
        if(v[pos + 1] != 0){
            pos = v[v[pos + 2]]
        }else {
            pos += 3
        }
    }else if(op == 1005) {
        if(v[v[pos + 1]] != 0){
            pos = v[pos + 2]
        }else {
            pos += 3
        }
    }else if(op == 1106) {
        if(v[pos + 1] == 0){
            pos = v[pos + 2]
        }else {
            pos += 3
        }
    }else if(op == 106) {
        if(v[pos + 1] == 0){
            pos = v[v[pos + 2]]
        }else {
            pos += 3
        }
    }else if(op == 1006) {
        if(v[v[pos + 1]] == 0){
            pos = v[pos + 2]
        }else {
            pos += 3
        }
    }else if(op == 7) {
        if(v[v[pos + 1]] < v[v[pos + 2]]){
            v[v[pos + 3]] = 1
        }else {
            v[v[pos + 3]] = 0
        }
        pos += 4
    }else if(op == 107) {
        if(v[pos + 1] < v[v[pos + 2]]){
            v[v[pos + 3]] = 1
        }else {
            v[v[pos + 3]] = 0
        }
        pos += 4
    }else if(op == 1107) {
        if(v[pos + 1] < v[pos + 2]){
            v[v[pos + 3]] = 1
        }else {
            v[v[pos + 3]] = 0
        }
        pos += 4
    }else if(op == 1007) {
        if(v[v[pos + 1]] < v[pos + 2]){
            v[v[pos + 3]] = 1
        }else {
            v[v[pos + 3]] = 0
        }
        pos += 4
    }else if(op == 8) {
        if(v[v[pos + 1]] == v[v[pos + 2]]){
            v[v[pos + 3]] = 1
        }else {
            v[v[pos + 3]] = 0
        }
        pos += 4
    }else if(op == 1108) {
        if(v[pos + 1] == v[pos + 2]){
            v[v[pos + 3]] = 1
        }else {
            v[v[pos + 3]] = 0
        }
        pos += 4
    }else if(op == 1008) {
        if(v[v[pos + 1]] == v[pos + 2]){
            v[v[pos + 3]] = 1
        }else {
            v[v[pos + 3]] = 0
        }
        pos += 4
    }else if(op == 108) {
        if(v[pos + 1] == v[v[pos + 2]]){
            v[v[pos + 3]] = 1
        }else {
            v[v[pos + 3]] = 0
        }
        pos += 4
    }else if(op == 3){
        v[v[pos + 1]] = 5
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
