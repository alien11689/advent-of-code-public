String text = new File('input.txt').text.trim()

List lines = text.split('\n').collect {it.trim()}

Map pos2Range = [:]

lines.each {
	List parts = it.split(':')
	int num = parts[0].trim() as int
	int range = parts[1].trim()  as int
	
	pos2Range[num] = range

}

/**pos2Range = [
(0): 3,
(1): 2,
(4): 4,
(6): 4
]*/

	def countPos(current, range, delay) {
		boolean down = true
		int start = 0
		int tick = 0
		return (current + delay) % (2 * (range -1)) == 0
	}

int delay = 0
	int max = pos2Range.keySet().max()
while(true){
	int myPos = 0
	Set caught = [] as Set
	++delay
	if(delay % 1000 == 0) {
		println delay
	}
while(myPos <= max){
	def range = pos2Range[myPos]
	if(range == null) { ++myPos; continue }
	if(countPos(myPos, range, delay)) {
		caught << myPos
		break
	}
	++myPos
}
if(caught.empty){
	println delay
	return
}
}

