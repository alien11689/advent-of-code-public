String text = new File('input.txt').text

List<List> grid = text.split('\n').collect {it as List}

def dir = [0, 1]
def cur = [grid[0].findIndexOf {it == '|'}, 0]

def letters = []

def changeDir(grid, dir, oldCur, cur){
	[
		[cur[0] + 1, cur[1]],
		[cur[0] - 1, cur[1]],
		[cur[0], cur[1] + 1],
		[cur[0], cur[1] - 1]
	].findAll {it != oldCur}
	.findAll { grid[it[1]][it[0]] != ' '}
	.collect { [it[0] - cur[0], it[1] - cur[1]] }
	.find()
	
}

while(dir){
	println "$cur -> ${grid[cur[1]][cur[0]]}"
	def oldCur = cur
	cur = [cur[0] + dir[0],cur[1] + dir[1]]
	def sign = grid[cur[1]][cur[0]]
	if(!(sign in ['-', '+', '|'])){
		letters << sign
		println letters.join()
	}
	if(sign == '+'){
		dir = changeDir(grid, dir, oldCur, cur)
	}
	if(sign == ' '){
		throw new Exception("Boom at $cur")
	}
}
