List<Long> values = new File('input.txt').text.split(',').collect { it as long} as ArrayList

int pos = 0
values[1] = 12
values[2] = 2

while(true){
    int op = values[pos]
    if(op == 99L){
        println values[0]
        return
    }
    if(op == 1){
        values[values[pos + 3]] = values[values[pos + 1]] + values[values[pos + 2]]
    }else {
        values[values[pos + 3]] = values[values[pos + 1]] * values[values[pos + 2]]
    }
    pos += 4
}
