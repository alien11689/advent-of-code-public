String text = new File('input.txt').text.trim()

List processSteps = text.split(',').collect {it.trim()}

@groovy.transform.ToString
class Point {
	BigDecimal x
	BigDecimal y

	Point(x,y){
		this.x = x
		this.y = y
	}
}


def dest = processSteps.inject(new Point( 0, 0)) { it, step -> 
	switch(step) {
		case "n": return new Point(it.x, it.y -1)
		case "ne": return new Point(it.x + 0.5, it.y - 0.5)
		case "nw": return new Point(it.x-0.5, it.y -0.5)
		case "s": return new Point(it.x, it.y +1)
		case "se": return new Point(it.x + 0.5, it.y +0.5)
		case "sw": return new Point(it.x - 0.5, it.y +0.5)
	}
}

println dest
println (dest.x / 0.5 + dest.y - dest.x) 
