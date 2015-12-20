
def text = new File('data5').text

def vowels = 'aeiou'

def appearsTwice(String input){
	def cur = ''
	for(s in input ){
		if(s == cur){
			return true
		}
		cur = s
	}
	return false
}

def hasForbidden(String input){
	def forbidden = ['ab', 'cd', 'pq', 'xy']
	forbidden.findAll {input.contains(it)}.size > 0
}

println text.split('\n')
	.findAll {
		String whole -> whole.findAll { vowels.contains(it) }.size() >= 3
	}.findAll { appearsTwice(it)}
	.findAll {!hasForbidden(it)}
	.size()

