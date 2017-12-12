String text = new File('input.txt').text.trim()

List routesDef = text.split('\n').collect {it.trim()}

Set routes = [] as Set

routesDef.each {
	List parts = it.split(' ', 3)
	int from = parts[0] as int
	List tos = parts[2].split(',').collect {it.trim() as int}
	println "From $from and to $tos "
}

