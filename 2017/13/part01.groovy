String text = new File('input.txt').text.trim()

List lines = text.split('\n').collect {it.trim()}

Map pos2Range = [:]

lines.each {
	List parts = it.split(':')
	pos2Range[parts[0] as int ] = parts[1] as int
}
println pos2Range
int tick = 0
int myPost = 0
Set caught = [] as Set

(0..(pos2Range.keys().max()).each { it -> 
	def range = pos2Range[it]
	if(range == null) { return }

}
 
