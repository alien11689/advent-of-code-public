List<Integer> input = new File('input.txt').text.trim().collect { it as int }
//input = "12345678".collect { it as int}
int phases = 100

List<Integer> basePattern = [0, 1, 0, -1]

int p = 0

List<Integer> generatePattern(List<Integer> base, int iter) {
    List<Integer> pattern = []
    base.each { p ->
        iter.times {
            pattern << p
        }
    }
    return pattern
}

while (p < phases) {
    ++p
    //    println("phase $p")
    int iter = 1
    List<Integer> output = []
    while (output.size() < input.size()) {
        List<Integer> pattern = generatePattern(basePattern, iter)
        int sum = 0
        for (int i = 0; i < input.size(); ++i) {
            sum += input[i] * pattern[(i + 1) % pattern.size()]
        }
        output << sum.abs() % 10
        ++iter
    }
    //println("$output")
    input = output
}
println("${input.subList(0, 8).join('')}")
