String text = new File('input.txt').text

List lines = text.split('\n').collect {it.trim()}

@groovy.transform.ToString
class Coord {
	int x,y,z
}

@groovy.transform.ToString
class Particle {
	Coord p
	Coord v
	Coord a
}

Coord buildCoords(str){
	List parts = str.split(',')
	new Coord(x: parts[0] as int, y: parts[1] as int , z: parts[2] as int)
}

def particles = lines.collect { line ->
	List parts = line.split('[<>]')
	new Particle(
		p: buildCoords(parts[1]),
		v: buildCoords(parts[3]),
		a: buildCoords(parts[5])
	)
}

particles.each {
	println it
}
