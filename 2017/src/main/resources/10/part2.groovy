String text = new File('input.txt').text.trim()
//String text = ''
//String text = 'AoC 2017'
//String text = '1,2,3'

List lengths = text.collect {(int) (it as char).charValue()}
println "Lengths: $lengths"
lengths.addAll([17, 31, 73, 47, 23])

int curPos = 0
int skipSize = 0

List input = (0..255).collect {it}

64.times {
	println "Iteration $it"
lengths.each { length ->
	List reversed = (0..<length).collect {
		input[(curPos+it) % input.size()]
	}.reverse()
	(0..<length).each {
		input[(curPos+it) % input.size()] = reversed[it]
	}
	curPos = (curPos + length + skipSize) % input.size()
	++skipSize
}
}

println (input.collate(16).collect {it.inject {a,b -> a ^ b}}.collect{String.format("%02x", it)}.join(''))

