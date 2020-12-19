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


int groups = 0

while(!routes.empty) {
	Set known = routes[0] as Set
	Set used = [routes[0]] as Set
	boolean search = true
	++groups
	while(search){

		search = false
		routes.findAll{!(it in used)}.each {
			if(it[0] in known || it[1] in known){
				search = true
				used << it
				known.addAll(it)
			}
		}
	}
	routes.removeAll(used)
}
println groups

