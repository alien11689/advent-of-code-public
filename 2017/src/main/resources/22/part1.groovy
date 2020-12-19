String text = new File('input.txt').text
//text = new File('input2.txt').text

List lines = text.split('\n').collect {it.trim()}

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
		Boolean value = grid[[x,y]]
		dir = value ? dir.right() : dir.left()
		grid[[x,y]] = !value
		if(grid[[x,y]]) {
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
			grid[[i, j]] = true
		}
		++i	
	}
	++j
		
}

def virus = new Virus(curX, curY)
println virus
10000.times {
	virus.burst(grid)
}

println virus.infected
