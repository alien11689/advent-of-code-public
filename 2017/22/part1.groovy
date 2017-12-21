String text = new File('input.txt').text

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
}

Map grid = [:]
int curPost = (lines[0].size() + 1) / 2

lines.each {
	
}
