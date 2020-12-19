def main(int n) {
	Set elvesWithoutPresent = [] as Set
	int i = 0
	while(elvesWithoutPresent.size() < n - 1){
		println i
		println "excluded: ${elvesWithoutPresent.size()}"
		int next = findNextWith(i, n, elvesWithoutPresent)
		elvesWithoutPresent << next
		i = findNextWith(i, n, elvesWithoutPresent)
	}
	return i + 1
}

def findNextWith(int cur, int all, Set ewp){
	int prev = cur
	while(true ) {
		int next = (prev + 1) % all
		if(next == cur || !(next in ewp)){
			return next
		}
		prev = next
	}
}

println(main(5))
println()

println(main(3017957))
