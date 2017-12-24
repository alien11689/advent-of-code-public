String input = new File('input.txt').text
//input = new File('input2.txt').text

List lines = input.split('\n').collect{it.trim()}

@groovy.transform.Immutable
class Cpu {
	List<Integer> pins
}

@groovy.transform.Immutable
class Chain {
	List<Cpu> cpus
	int lastPin

	Chain connectTo(Cpu c){
		if(c in cpus){
			return null
		}
		if(!(lastPin in c.pins)){
			return null
		}
		return new Chain(cpus + [c], lastPin == c.pins[0]? c.pins[1] : c.pins[0])
	}

	int length(){
		return cpus.collectMany {it.pins}.sum()
	}
}

List cpus = lines.collect { 
	new Cpu(it.split('/').collect {it as int})
}

List chains = []
Stack stack = new Stack()
stack.push(new Chain([], 0))

while(!stack.empty){
	Chain parent = stack.pop()
	List newChains = cpus.collect {
		parent.connectTo(it)
	}.findAll()
	chains.addAll newChains
	newChains.each {
		stack.push(it)
	}
}

println chains.collect {it.length()}.max()
