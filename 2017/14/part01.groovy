def input = "flqrgnkx"

def knotHash(text){
List lengths = text.collect {(int) (it as char).charValue()}
lengths.addAll([17, 31, 73, 47, 23])

int curPos = 0 
int skipSize = 0 

List input = (0..255).collect {it}

64.times {
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

return (input.collate(16).collect {it.inject {a,b -> a ^ b}}.collect{String.format("%02x", it)}.join(''))	
}

def grid =  ((0..127).collect {"$input-$it"}.collect {knotHash(it)})
	.collect {
		it.collect {
			"0x$it"
		}
	}


