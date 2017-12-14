def input = "flqrgnkx"
input = "ugkiagan"

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

def map = [
	'0':'0000',
	'1':'0001',
	'2':'0010',
	'3':'0011',
	'4':'0100',
	'5':'0101',
	'6':'0110',
	'7':'0111',
	'8':'1000',
	'9':'1001',
	'a':'1010',
	'b':'1011',
	'c':'1100',
	'd':'1101',
	'e':'1110',
	'f':'1111',
]

List<List<String>> grid =  ((0..127).collect {"$input-$it"}.collect {knotHash(it)})
	.collect {
		it.collect {
			map[it]
		}.join('') as List
	}

int currentGroup = 0

def neighbour(i,j, size) {
	return [
		[i -1, j],
		[i+1, j],
		[i, j-1],
		[i, j+1]
	].findAll {it[0] >= 0 && it[0] < size && it[1] >= 0 && it[1] < size}
}

(0..<(grid.size())).each { i->
	(0..<(grid[0].size())).each { j->
		if(grid[i][j] == '1'){
			int val = currentGroup--
			def mem = [] as Set
			List neighbours = [[i,j]]
			while(!neighbours.empty){
					def cur = neighbours.pop()
					if(grid[cur[0]][cur[1]] == '1'){
						grid[cur[0]][cur[1]] = "$val"
						def n = neighbour(cur[0], cur[1], grid.size()).findAll {! (it in mem)}
						mem.addAll(n)
						neighbours.addAll(n)
					}

			}

		}
	}
}

println (currentGroup * (-1))
