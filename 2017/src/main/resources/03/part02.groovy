int input = 277678
//int input = 805

enum Dir {
	R(1, 0),
	U(0, -1),
	L(-1, 0),
	D(0, 1)

	final int x
	final int y

	Dir(xx,yy){
		x = xx
		y = yy
	}
}

import static Dir.*

def nextDir = [
	(R): U,
	(U): L,
	(L): D,
	(D): R
]

@groovy.transform.Immutable
class Cur {
	int x
	int y

	def plus(Dir d){
		return new Cur(x + d.x, y + d.y)
	}
}

def mesh = [:]
def cur = new Cur(0, 0)
def dir = R
def last = 1
mesh[cur] = last

def sumNeighbours(mesh, cur){
	[cur + L, cur + L + U, cur + L + D, cur + U, cur + D, cur + R, cur + R + U, cur + R + D].collect {
		mesh[it]
	}.findAll().sum()
}

while(last <= input){
	println cur
	cur = cur + dir
	last = sumNeighbours(mesh,cur)
	mesh[cur] = last
	if(!mesh[cur + nextDir[dir]]){

		dir = nextDir[dir]
	}
}
println cur
println last
