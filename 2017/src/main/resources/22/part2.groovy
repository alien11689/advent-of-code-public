String text = new File('input.txt').text
//text = new File('input2.txt').text

List lines = text.split('\n').collect {it.trim()}

enum Status {
	Clean,
	Weakened,
	Infected,
	Flagged

	def next() {
		switch(this){
			case Clean: return Weakened
			case Weakened: return Infected
			case Infected: return Flagged
			case Flagged: return Clean
		}
	}

}

enum Dir {
	Left(-1, 0),	
	Right(1, 0),	
	Up(0, -1),	
	Down(0, 1)

	final int x,y
	Dir(xx,yy){
		x = xx
		y = yy
	}

	def left(){
		switch(this) {
			case Left: return Down
			case Up: return Left
			case Right: return Up
			case Down: return Right
		}
	}

	def right(){
		switch(this) {
			case Left: return Up
			case Up: return Right
			case Right: return Down
			case Down: return Left
		}
	}

	def reverse(){
		switch(this) {
			case Left: return Right
			case Up: return Down
			case Right: return Left
			case Down: return Up
		}
	}
}

Map grid = [:]

@groovy.transform.ToString
class Virus {
	int x
	int y
	Dir dir = Dir.Up
	int infected = 0

	Virus(xx,yy) {
		x = xx
		y = yy
	}

	def burst(grid) {
		def value = grid[[x,y]] ?: Status.Clean
		switch(value) {
			case Status.Clean: dir = dir.left(); break
			case Status.Weakened: break
			case Status.Infected: dir = dir.right(); break
			case Status.Flagged: dir = dir.reverse(); break
		}
		grid[[x,y]] = value.next()
		if(grid[[x,y]] == Status.Infected) {
			infected++
		}
		x += dir.x
		y += dir.y
	}
}

int curX = (lines[0].size()) / 2
int curY = (lines.size()) / 2

int j = 0
lines.each { row ->
	int i = 0
	(row as List).each { cell ->
		if(cell == '#'){
			grid[[i, j]] = Status.Infected
		}
		++i	
	}
	++j
		
}

def virus = new Virus(curX, curY)
println virus
10000000.times {
	virus.burst(grid)
}

println virus.infected
