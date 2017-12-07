def text = new File('input.txt').text

List lines = text.split('\n').collect {it.trim()}

class Disc {
	String name
	int size
	List children
}

Map<String, Disc> discs = [:]

lines.each {line ->
	println line.split(' ', 4)

}
