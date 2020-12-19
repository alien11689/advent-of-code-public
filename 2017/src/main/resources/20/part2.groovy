String text = new File('input.txt').text

List lines = text.split('\n').collect {it.trim()}

@groovy.transform.ToString
@groovy.transform.EqualsAndHashCode
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
	Map hits = [:].withDefault {0}
	particles.each {
		it.updateV()
		it.updateP()
		hits[it.p] = 1 + hits[it.p]
	}
	Set doubles = hits.findAll {it.value > 1}.keySet()
	particles = particles.findAll { !(it.p in doubles) }
	println "In iter $iter -> ${particles.size()}"
}
