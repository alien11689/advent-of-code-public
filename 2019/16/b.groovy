List<Integer> input = new File('input.txt').text.trim().collect { it as int }
//input = '03036732577212944063491565474664'.collect { it as int }
//input = '02935109699940807407585447034323'.collect { it as int }
//input = '03081770884921959731165446850517'.collect { it as int }
int offset = input.subList(0, 7).dropWhile { it == 0 }.collect { it as String }.join('') as int

int times = 10000 - ((int) (offset / input.size()))
//println("Times $times")
int phases = 100
offset = offset % input.size()

List<Integer> realInput = []
times.times {
    realInput.addAll(input)
}

input = realInput

int p = 0

while (p < phases) {
    ++p
//    println("phase $p")
    long sum = input.inject(0L) { a, b -> a + b }
    List<Integer> output = []
    for (int i = 0; i < input.size(); ++i) {
        if (i > 0) {
            sum -= input[i - 1]
        }
        output << sum % 10
    }
    input = output
}
println("${input.subList(offset, offset + 8).join('')}")
