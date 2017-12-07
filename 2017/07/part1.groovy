def text = new File('input.txt').text

List lines = text.split('\n').collect {it.trim()}

class Disc {
	String name
	int size
	List children
}

Map<String, Disc> discs = [:]
Set<String> allChildren = [] as Set

lines.each {line ->
	List parts = line.split(' ', 4)
	println parts
	def disc = new Disc(
                name : parts[0],
                size: parts[1].replace('(', '').replace(')', '') as int,
                children: parts.size() > 2 ? parts[3].split(',').collect {it.trim()} : []  
        ) 
	discs[parts[0]] = disc
	allChildren.addAll(disc.children)
}

discs.keySet().each { 
	if(!(it in allChildren)){
		println it
	}
}
