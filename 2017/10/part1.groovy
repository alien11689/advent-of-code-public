String text = new File('input.txt').text
List lengths = text.split(',').collect {it as int}

int curPos = 0
int skipSize = 0

List input = (0..255).collect {it}

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

println lengths
println input
println (input[0] * input[1])

