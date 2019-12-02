List<Long> v = new File('input.txt').text.split(',').collect { it as long} as ArrayList

for(int i = 0; i < 100; ++i){
    for(int j = 0; j < 100; ++j){
        List<Long> values = v.clone()
        values[1] = i
        values[2] = j
        long solution = solve(values)
        if(solution == 19690720){
            println(i*100 + j)
            return
        }
    }
}

def solve(List<Long> values){
int pos = 0
while(true){
    int op = values[pos]
    if(op == 99L){
        return values[0]
    }
    if(op == 1){
        values[values[pos + 3]] = values[values[pos + 1]] + values[values[pos + 2]]
    }else {
        values[values[pos + 3]] = values[values[pos + 1]] * values[values[pos + 2]]
    }
    pos += 4
}
}
