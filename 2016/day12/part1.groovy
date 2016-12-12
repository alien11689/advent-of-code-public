def main(String input) {
    def commands = new File(input).text
        .split('\n')
	.collect {parseCommand (it)}
    Map reg = [a: 0, b:0, c:0, d: 0, e: 0]
    int i = 0
    println commands
    while(i < commands.size()){
	def command = commands[i]
	i += command.apply(reg)	    
    }
    println reg
}

class Cpy {
	String from
	String to	

	def apply(Map reg){
		if(from in reg.keySet()){
			reg[to] = reg[from]	
		}else {
			reg[to] = from as int	
		}
		1
	}
}

class Inc {
	String who
	def apply(Map reg){
		++reg[who]	
		1
	}	
}
class Dec {
	String who
	def apply(Map reg){
		--reg[who]	
		1
	}	
}

class Jnz {
	String decider
	int jump
	def apply(Map reg){
		int dec = decider in reg.keySet() ? reg[decider] : (decider as int)
		dec != 0 ? jump : 1
	}	
}

def parseCommand(String line){
	String[] split= line.split(' ')
	if(split[0] == 'cpy'){
		return new Cpy(from: split[1], to: split[2])	
	}else if(split[0] == 'inc'){
		return new Inc(who: split[1])	
	}else if(split[0] == 'dec'){
		return new Dec(who: split[1])	
	}else if(split[0] == 'jnz'){
		return new Jnz(decider: split[1], jump: split[2] as int)	
	}else {
		throw new Exception()	
	}
}

(main('sample.txt'))
println()
println(main('input.txt'))
