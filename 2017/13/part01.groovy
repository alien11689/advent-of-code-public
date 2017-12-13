String text = new File('input.txt').text.trim()

List lines = text.split('\n').collect {it.trim()}

Map pos2Range = [:]

lines.each {
	List parts = it.split(':')
	int num = parts[0].trim() as int
	int range = parts[1].trim()  as int
	
	pos2Range[num] = range

}

int tick = 0
int myPos = 0
Set caught = [] as Set
int max = pos2Range.keySet().max()

while(myPos <= max){
	println "$myPos/$max"
	def range = pos2Range[myPos]
	if(range == null) { ++myPos; continue }
	if(myPos % range == 0) {
		caught << myPos
	}
	++myPos
}

int sum = caught.collect {pos2Range[it] * it}.sum()

println sum

