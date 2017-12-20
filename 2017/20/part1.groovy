String text = new File('input.txt').text

List lines = text.split('\n').collect {it.trim()}

@groovy.transform.ToString
class Coord {
	int x,y,z
}

@groovy.transform.ToString
class Particle {
	int id
	Coord p
	Coord v
	Coord a

	def updateV(){
		v = new Coord(
			x: v.x + a.x,
			y: v.y + a.y,
			z: v.z + a.z,
			)
	}
	def updateP(){
		p = new Coord(
			x: v.x + p.x,
			y: v.y + p.y,
			z: v.z + p.z,
			)
	}
	def from0(){
		return p.x.abs() + p.y.abs() + p.z.abs()
	}
}

Coord buildCoords(str){
	List parts = str.split(',')
	new Coord(x: parts[0] as int, y: parts[1] as int , z: parts[2] as int)
}

int id = 0
def particles = lines.collect { line ->
	List parts = line.split('[<>]')
	new Particle(
		id: id++,
		p: buildCoords(parts[1]),
		v: buildCoords(parts[3]),
		a: buildCoords(parts[5])
	)
}

particles.each {
	println it
}

1000.times {iter ->
	print "In iter $iter -> "
	println (particles.each {
		it.updateV()
		it.updateP()
	}.min { it.from0() }.id)
}
