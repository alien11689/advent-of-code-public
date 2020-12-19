def main(String input) {
	def nodes = input.split('\n')
		.drop(2)
		.collect {fromLine(it)}

	[nodes,nodes].combinations()
		.findAll {
			it[0] != it[1] && 
				it[0].used > 0 && 
				it[0].used <= it[1].avail
		}.sum {1}

}

@groovy.transform.EqualsAndHashCode
@groovy.transform.ToString
class Node {
	int x 
	int y
	int size
	int used
	int avail
	int percent
}

def fromLine(String line){
	def split = line.split(/\s+/)
	def coords = split[0].split(/-/)
	new Node(
		x: coords[1][1..-1] as int,
		y: coords[2][1..-1] as int,
		size: split[1][0..<-1] as int,
		used: split[2][0..<-1] as int,
		avail: split[3][0..<-1] as int,
		percent: split[4][0..<-1] as int,
	)
}

println(main(new File('input.txt').text))
