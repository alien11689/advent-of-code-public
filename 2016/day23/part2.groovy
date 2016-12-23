def main(String input) {
    def commands = new File(input).text
        .split('\n')
	.collect {parseCommand (it)}
    Map reg = [a: 12l, b:0l, c:0l, d: 0l]
    int i = 0
    println commands
    while(i < commands.size()){
	println "$i: $reg ${commands[i]}"
	if(i == 5 && reg.b > 0 && reg.c > 0 && reg.d> 0){
		i = 7
		reg.a = reg.c  * reg.d
		reg.c = 0l
		reg.d = 1l
	}else{
		def command = commands[i]
		i += command.apply(reg, commands, i)
	}
    }
    println reg
}

@groovy.transform.ToString
class Cpy {
	String from
	String to	
	int arguments = 2
	def apply(Map reg, def commands, def current){
		if(from in reg.keySet()){
			reg[to] = reg[from]	
		}else {
			reg[to] = from as long
		}
		1
	}
}

@groovy.transform.ToString
class Tgl {
	String who
	int arguments = 1

	def apply(Map reg, def commands, def current){
		int jump
		if(who in reg.keySet()){
			jump = reg[who]
		}else {
			jump = who as int	
		}
		int toChange = jump + current
		if(toChange >= commands.size() || toChange < 0){
			return 1	
		}
		def command = commands[toChange]
		if(command.arguments == 1){
			if (command instanceof Inc){
				commands[toChange] = new Dec(who:command.who)	
			}else{
				commands[toChange] = new Inc(who: command.who)	
			}
		}else{
			if(command instanceof Jnz){
				commands[toChange] = new Cpy(from: command.decider, to: command.jump)
			}else{
				commands[toChange] = new Jnz(decider: command.from , jump: command.to)	
			}
		}
		return 1
	}
}

@groovy.transform.ToString
class Inc {
	String who
	int arguments = 1
	def apply(Map reg, def commands, def current){
		++reg[who]	
		1
	}	
}
@groovy.transform.ToString
class Dec {
	int arguments = 1
	String who
	def apply(Map reg, def commands, def current){
		--reg[who]	
		1
	}	
}

@groovy.transform.ToString
class Jnz {
	int arguments = 2
	String decider
	String jump
	def apply(Map reg, def commands, def current){
		long dec = decider in reg.keySet() ? reg[decider] : (decider as long)
		long j = jump in reg.keySet() ? reg[jump] : (jump as int)
		dec != 0 ? j : 1
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
		return new Jnz(decider: split[1], jump: split[2])	
	}else if(split[0] == 'tgl'){
		return new Tgl(who: split[1])	
	}else {
		throw new Exception()	
	}
}

println(main('sample.txt'))
println()
println(main('input.txt'))
