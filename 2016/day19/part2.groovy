def main(int n) {
	List elves = (1..n).collect {it}
	int i = 0
	while(elves.size() > 1){
		int across = findAcross(i, elves)
		println "Elves: ${elves.size()},  Current $i: -> elf ${elves[i]}, Across $across: -> elf ${elves[across]}"
		elves.remove(across)
		if(across > i){
			++i
		}
		i = i >= elves.size() ? 0 : i
	}
	return elves.first()
}

def findAcross(int cur, List elves){
	int size = elves.size()
	int step = (size % 2 == 1 ? size - 1 : size) / 2
	(cur + step) % size
}

println(main(5))
println()

println(main(3017957))
