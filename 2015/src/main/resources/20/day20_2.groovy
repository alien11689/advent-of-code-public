int input = 29000000

int [] a = (0..<input).collect {0} as int []
(1..input).each {
//    println it
    for(int i = it - 1; i < input && i < it * 50; i += it){
        a[i] += it * 11
    }
    if(a[it-1] >= input){
        println it
        throw new RuntimeException ("$it")
    }
}
