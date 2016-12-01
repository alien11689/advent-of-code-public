
def text = new File('day5.data').text

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

def pairReapeated(String input){
    def found = false
    println "Checking $input"
    input[0..<-3].eachWithIndex { elem, i -> 
        String search = elem + input[i+1]
        if(input[i+2..-1].indexOf(search) >= 0){
            found = true
        } 
    }
    if(found){
        println input
    }
    found
}

def repeatWithBetween(String input){
    def found = false
    input[0..<-2].eachWithIndex { elem, i -> 
        String third = input[i + 2]
        found = found || elem == third
    }
    found
}

println text.split('\n')
//	.findAll {
//		String whole -> whole.findAll { vowels.contains(it) }.size() >= 3
//	}.findAll { appearsTwice(it)}
//	.findAll {!hasForbidden(it)}
	.findAll {pairReapeated(it)}
	.findAll {repeatWithBetween(it)}
	.size()

