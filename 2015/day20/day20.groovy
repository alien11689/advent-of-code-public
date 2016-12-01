int input = 29000000

int [] a = (0..<input).collect {10} as int []
(2..input).each {
    println it
    for(int i = it - 1; i < input; i += it){
        a[i] += it * 10 
    }
    if(a[it-1] >= input){
        println it
        throw new RuntimeException ("$it")
    }
}
