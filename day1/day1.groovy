String input = new File('day1.data').text

println input.inject(0){acc, cur ->
    if(cur == '('){
        acc + 1
    }else if(cur == ')'){
        acc -1
    }else{
        acc
    }
}
