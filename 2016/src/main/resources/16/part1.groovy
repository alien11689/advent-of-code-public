def main(String input, int size) {
	String text = input
	while(text.size() < size){
		text = step(text)
		println "part: $text"
	}
	checksum(text.take(size))
}

def step(String a){
	String b = a.reverse().collect {it == '0' ? '1' : '0'}.join('')
	a + '0' + b
}

def checksum(String input){
	String text = input
	while(text.size() % 2 == 0){
		text = text.collect {it}.collate(2).collect { it[0] == it[1] ? '1' : '0'}.join('')
		println "Chacksum: $text"
	}
	text
}

println(main('10000', 20))
println()

println(main('00111101111101000', 272))
