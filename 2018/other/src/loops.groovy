import java.time.Duration
import java.time.LocalTime

long n = args.size() > 0 ? args[0] as long : 10

void printAddingTime(String message, long to, Closure<Long> adder) {
    LocalTime start = LocalTime.now()
    long sum = adder(to)
    println("$message: $sum calculated in ${Duration.between(start, LocalTime.now()).toMillis()} ms")
}

printAddingTime('for with sum', n) {
    long sum = 0
    for (long i = 1; i <= n; ++i) {
        for (long j = 1; j <= n; ++j) {
            sum += i * j
        }
    }
    sum
}

printAddingTime('while with sum', n) {
    long sum = 0
    long i = 1
    while(i <= n){
	long j = 0
    	while(j <= n){
	    sum+= i*j
	    ++j
    	}
	++i
    }
    sum
}

printAddingTime('times', n) {
    long sum = 0
    n.times {long i-> n.times { long j -> sum += (i + 1)*(j+1)}}
    sum
}

printAddingTime('range collect and each', n) {
    long sum = 0
    (1..n).each { i ->
        (1..n).each { j ->
            sum += i * j
        }
    }
    sum
}

printAddingTime('range collect and sum', n) {
    (1..n).collect { i -> (1..n).collect { j -> i * j }.sum() }.sum()
}
