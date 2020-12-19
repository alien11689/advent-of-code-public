String text = new File('input.txt').text.trim()

List routesDef = text.split('\n').collect {it.trim()}

List routes = []

routesDef.each {
	List parts = it.split(' ', 3)
	int from = parts[0] as int
	List tos = parts[2].split(',').collect {it.trim() as int}
	tos.each {
		routes << [from, it]
	}
}

Set know0 = [0]
Set used = [] as Set
boolean search = true

while(search) {
	search = false
	routes.findAll{!(it in used)}.each {
		if(it[0] in know0 || it[1] in know0){
			search = true
			used << it
			know0.addAll(it)
		}
	}
}
println know0.size()

