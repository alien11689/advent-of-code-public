String text = new File('input.txt').text
//text = new File('input2.txt').text
List actions = text.split(',').collect {it.trim()}

int range = 15

List a = (0..range).collect {
	(('a' as char ) + it) as char
}

class Partner {
	int n1
	int n2

def apply(a){
	Map toEx = [:]
	(0..<(a.size())).each{ i ->
		if(a[i] == n1){
			toEx[n1] = i
		}
		if(a[i] == n2){
			toEx[n2] = i
		}
	}
	return new Exchange(p1: toEx[n1], p2: toEx[n2]).apply(a)
}

}

class Spin{
	int s

def apply(a) {
	List newA = []
	newA.addAll(a[(-1 * s)..-1])
	newA.addAll(a[0..(-1 * s - 1 )])
	return newA
}
}

class Exchange {
	int p1
	int p2

def apply(a) {
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
}

List acc = actions.collect { action ->
                if(action[0] == 's'){
                        int nums = action[1..-1] as int 
			new Spin(s: nums)
                } else if(action[0] == 'x'){
                        List parts = action[1..-1].split('/')
                        new Exchange(p1: parts[0] as int, p2: parts[1] as int)
                } else if(action[0] == 'p'){
                        List parts = action[1..-1].split('/')
                        new Partner(n1: parts[0], n2: parts[1])
                }
        }  

long iter = 0
def mem = [:]

def all = 1000000000
while(iter < all){
	++iter
	println iter
	a = acc.inject(a) {cur, action -> cur = action.apply(cur)}
	if(a in mem){
		int diff = iter - mem[a]
		int match = (all - iter)/diff
		iter += match * diff
		/**while(iter < all){
			println "Skipping $diff"
			iter += diff
			println "Iter is now $iter"
		}*/
	}
	mem[a] = iter
}
println (a.join(''))
