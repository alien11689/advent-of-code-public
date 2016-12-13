def main(int magic, int destX, int destY) {
	def isOpenSpace = { int x, int y -> 
		Integer.toBinaryString(x*x + 3*x + 2*x*y + y + y*y + magic).replaceAll(/0/, '').size() % 2 == 0
	}
	Stage s = new Stage(posX:1, posY: 1, step:0)
	Queue q = new LinkedList()
	def mem = [] as Set 
	mem.add([1,1])
	q << s

	while(!q.empty){
		Stage cur = q.poll()
		println cur
		mem << [cur.posX, cur.posY]
		if(cur.posX == destX && cur.posY == destY) {
			return cur.step	
		}
		q.addAll (cur.generateNeighbors(isOpenSpace).findAll {!([it.posX, it.posY] in mem)})
	}
	
}

@groovy.transform.ToString
class Stage {
	int posX
	int posY
	int step
	
	def generateNeighbors(Closure isOpenSpace){
		def next = step+1
		def xs = [posX -1 ,posX +1].findAll {it >= 0 && isOpenSpace(it, posY)}.collect { new Stage(posX: it, posY: posY, step: next)}
		def ys = [posY -1 ,posY +1].findAll {it >= 0 && isOpenSpace(posX, it)}.collect { new Stage(posX: posX, posY: it, step: next)}
		xs + ys
	}
}



println(main(10, 7, 4))
println()
println(main(1350, 31, 39))
