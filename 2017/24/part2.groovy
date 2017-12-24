String input = new File('input.txt').text
//input = new File('input2.txt').text

List lines = input.split('\n').collect{it.trim()}

@groovy.transform.Immutable
class Cpu {
	List<Integer> pins
}

@groovy.transform.ToString
@groovy.transform.EqualsAndHashCode
class Chain {
	List<Cpu> cpus
	int lastPin
	int length
	int strength

	Chain(c, l){
		cpus = c
		lastPin = l
		length = c.size()
		strength = c.collectMany {it.pins}.sum() ?: 0
	}

	Chain connectTo(Cpu c){
		if(c in cpus){
			return null
		}
		if(!(lastPin in c.pins)){
			return null
		}
		return new Chain(cpus + [c], lastPin == c.pins[0]? c.pins[1] : c.pins[0])
	}
}

List cpus = lines.collect { 
	new Cpu(it.split('/').collect {it as int})
}

Map l2sizes = [:].withDefault {[]}
Queue queue = new LinkedList()
queue.offer(new Chain([], 0))

while(!queue.empty){
	Chain parent = queue.poll()
	l2sizes[parent.length] = l2sizes[parent.length] + parent.strength
	List newChains = cpus.collect {
		parent.connectTo(it)
	}.findAll().each {
		queue.offer(it)
	}
}

int maxLength = l2sizes.keySet().max()
println l2sizes[maxLength].max()

