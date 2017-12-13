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

memory = [:]
	
	def countPos(current, range, delay) {
		boolean down = true
		int start = 0
		int tick = 0
		def fromMem = memory[[current, current + delay - 1]]
		if(fromMem != null){
			start = fromMem[0]
			down = fromMem[1]
			tick = current + delay - 1
		}
		while(tick < current + delay){
			println "Laser $current is in pos $start/$range with delay $delay"
			start += (down ? 1 : -1)
			if(start == range - 1){
				down = false
			}
			if(start == 0) {
				down = true
			}
			++tick
		}
		memory[[current, tick]] = [start, down]
		return start
	}

int delay = 3000
	int max = pos2Range.keySet().max()
while(true){
	int myPos = 0
	Set caught = [] as Set
	++delay
while(myPos <= max){
	def range = pos2Range[myPos]
	if(range == null) { ++myPos; continue }
	if(countPos(myPos, range, delay) == 0) {
		println "$myPos/$max caught"
		caught << myPos
		break
	}else {
		
		println "$myPos/$max not caught"
	}
	++myPos
}
if(caught.empty){
	
	println delay
	return
}
}

