def text = new File('input.txt').text
//def text = "0	2	7	0"
List blocks = text.split('\t').collect {it as int}
Map memory = [(blocks.collect()): 0]
int re = 0


def findMax(List b){
	int max = b[0]
	int maxI = 0
	for(int i = 1; i < b.size(); ++i){
		if(b[i] > max) {
			max = b[i]
			maxI = i
		}
	}	
	return [max, maxI]
}

while(true){
	println blocks
	re++
	def (max, maxI) = findMax(blocks)
	blocks[maxI] = 0
	while(max > 0) {
		--max
		maxI = (maxI + 1) % blocks.size()
		blocks[maxI]++
	}
	if(blocks in memory){
		println (re - memory[blocks])
		break
	}
	memory[blocks.collect()] = re
}

