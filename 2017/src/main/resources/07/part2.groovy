def text = new File('input.txt').text

List lines = text.split('\n').collect {it.trim()}
Map<String, Disc> discs = [:]

@groovy.transform.ToString
class Disc {
	String name
	int size
	List children

	private int sum = -1

	int wholeSize(discs){
		if(sum >= 0) {
			return sum
		}
		sum = size + (children.collect {discs[it].wholeSize(discs)}.sum() ?: 0)
		return sum
	}

	boolean hasBalancedChildren(discs){
		if(children.empty) {
			return false
		}
		List sizes = children.collect {discs[it].wholeSize()}
		if((sizes as Set).size() != 1) {
			println "$name has unbalanced children: $sizes"
			return true
		}
		return false
	}
}

Set<String> allChildren = [] as Set

lines.each {line ->
	List parts = line.split(' ', 4)
	def disc = new Disc(
                name : parts[0],
                size: parts[1].replace('(', '').replace(')', '') as int,
                children: parts.size() > 2 ? parts[3].split(',').collect {it.trim()} : []  
        ) 
	discs[parts[0]] = disc
	allChildren.addAll(disc.children)
}

discs.each { k,v -> v.wholeSize(discs)}
discs.findAll { k,v -> v.hasBalancedChildren(discs)}.each {k,v -> println v; println v.children.each { println " $it -> ${discs[it].size} and whole ${discs[it].wholeSize(discs)}"}}
