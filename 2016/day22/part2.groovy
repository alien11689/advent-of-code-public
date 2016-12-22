def main(String input) {
	def nodes = input.split('\n')
		.drop(2)
		.collect {fromLine(it)}
		.collectEntries {
			[[it.x,it.y]: it]	
		}
	def memory = [] as Set
	Queue q = new LinkedList()
	Stage init = new Stage(
		count: 0,
		data: new Data(
			nodes: nodes,
			x: nodes.values().max{it.x}.x,
			y: 0
		)
	)
	q << init
	memory << init.data.nodes
	printNodes(nodes)
	return
	while(q.size() > 0){
		Stage stage = q.poll()
		println "Size: ${q.size() + 1} -> Count: ${stage.count}"
		if(stage.data.x == 0 && stage.data.y == 0) {
			return stage.count
		}
		def moves = moves(stage.data.nodes)
		moves.collect {
			def a = it[0]
			def b = it[1]
			def newA = new Node(x: a.x, y: a.y, used: 0, avail: a.avail + a.used)
			def newB = new Node(x: b.x, y: b.y, used: b.used + a.used, avail: b.avail - a.used)
			def newNodes = stage.data.nodes.collectEntries { it.value == a ? [[newA.x,newA.y]: newA] : (it.value == b ? [[newB.x,newB.y]:newB] : it)}
			def data = a.x == stage.data.x && a.y == stage.data.y ? new Data (
				nodes:newNodes,
				x: b.x,
				y: b.y
			) : new Data (
				nodes:newNodes,
				x: stage.data.x,
				y: stage.data.y
			)
			if(data.nodes in memory){
				null	
			}else {
				memory << data.nodes
				new Stage(data: data, count: stage.count + 1)	
			}
		}.findAll().each { q << it}
	}
}

@groovy.transform.EqualsAndHashCode
class Data {
	def nodes
	int x
	int y
}

@groovy.transform.EqualsAndHashCode
class Stage {
	Data data
	int count	
}

def printNodes(nodes){
	def maxX = nodes.values().max{it.x}.x
	def maxY = nodes.values().max{it.y}.y
	for(int y = 0; y <= maxY; ++y){
		for(int x = 0; x <= maxX; ++x){
			print(nodes[[x,y]].toString() + "\t")
		}
		println()
	}
	
}

def moves(nodes){
	nodes.findAll {
		it.value.used > 0
	}.collectMany { cur ->
		def (x,y) = cur.key 
		[
			nodes[[x+1,y]],
			nodes[[x-1,y]],
			nodes[[x,y+1]],
			nodes[[x,y-1]],
		].findAll {it && cur.value.used <= it.avail}
		.collect {[cur.value, it]}
	} 
}

@groovy.transform.EqualsAndHashCode
class Node {
	int x 
	int y
	int used
	int avail

	String toString(){"$used/$avail"}
}

def fromLine(String line){
	def split = line.split(/\s+/)
	def coords = split[0].split(/-/)
	new Node(
		x: coords[1][1..-1] as int,
		y: coords[2][1..-1] as int,
		used: split[2][0..<-1] as int,
		avail: split[3][0..<-1] as int,
	)
}

println(main(new File('input.txt').text))
