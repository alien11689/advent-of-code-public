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
]
*/

int myPos = 0
Set caught = [] as Set
int max = pos2Range.keySet().max()
	
	def countPos(current, range) {
		boolean down = true
		int start = 0
		int tick = 0 
		while(tick < current){
			println "Laser $current is in pos $start/$range"
			start += (down ? 1 : -1)
			if(start == range - 1){
				down = false
			}
			if(start == 0) {
				down = true
			}
			++tick
		}
		return start
	}

while(myPos <= max){
	def range = pos2Range[myPos]
	if(range == null) { ++myPos; continue }
	if(countPos(myPos, range) == 0) {
		println "$myPos/$max caught"
		caught << myPos
	}else {
		
		println "$myPos/$max not caught"
	}
	++myPos
}

int sum = caught.collect {pos2Range[it] * it}.sum()

println sum

