def main(String input, int size) {
	List text = input.toCharArray()
	while(text.size() < size){
		text = step(text)
		println "part: ${text.size()}"
	}
	checksum(text.take(size)).join('')
}

def step(List a){
	List b = a.reverse().collect {it == '0' ? '1' as char : '0' as char}
	a + ['0' as char] + b
}

def checksum(List text){
	while(text.size() % 2 == 0){
		text = text.collect {it}.collate(2).collect { it[0] == it[1] ? '1' as char : '0' as char}
		println "Chacksum: ${text.size()}"
	}
	text
}

println(main('10000', 20))
println()

println(main('00111101111101000', 35651584))
