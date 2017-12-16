String text = new File('input.txt').text
//text = new File('input2.txt').text
List actions = text.split(',').collect {it.trim()}

int range = 15

List a = (0..range).collect {
	(('a' as char ) + it) as char
}

def spin(a, s) {
	List newA = []
	newA.addAll(a[(-1 * s)..-1])
	newA.addAll(a[0..(-1 * s - 1 )])
	return newA
}

def exchange(a, p1, p2) {
	List newA = []
	(0..<(a.size())).each{ i ->
		if(i == p1) {
			newA << a[p2]
		}else if(i == p2){
			newA << a[p1]
		}else{
			newA << a[i]
		}
	}
	return newA
}

def partner(a, n1, n2){
	Map toEx = [:]
	(0..<(a.size())).each{ i ->
		if(a[i] == n1){
			toEx[n1] = i
		}
		if(a[i] == n2){
			toEx[n2] = i
		}
	}
	return exchange(a, toEx[n1], toEx[n2])
}

	a = actions.inject(a) {cur, action ->
		if(action[0] == 's'){
			int nums = action[1..-1] as int
			spin(cur, nums)
		} else if(action[0] == 'x'){
			List parts = action[1..-1].split('/')
			exchange(cur, parts[0] as int, parts[1] as int)
		} else if(action[0] == 'p'){
			List parts = action[1..-1].split('/')
			partner(cur, parts[0], parts[1])
		}
	}
println (a.join(''))
