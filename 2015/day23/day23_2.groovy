String input = new File('day23.data').text

class Instr {
	String name
	String reg
	Integer param	
}

List<Instr> instructions = input.split('\n').collect {
	def s = it.replaceAll(',', '').split(' ')
	new Instr(name: s[0], reg: s[1], param: (s.size() != 3 ? null : (s[2] as int)))
}

i = 0

regs = [a: 1, b:0]

while(i < instructions.size()){
	def instr = instructions[i]
	if(instr.name == 'hlf'){
		regs[instr.reg] *= 0.5
		++i
	}else if(instr.name == 'tpl'){
		regs[instr.reg] *= 3
		++i
	}else if(instr.name == 'inc'){
		regs[instr.reg] += 1
		++i
	}else if(instr.name == 'jmp'){
		i+= instr.reg as int
	}else if(instr.name == 'jie'){
		i += ((regs[instr.reg] as int) % 2 == 0 ? instr.param : 1)
	}else if(instr.name == 'jio'){
		i += (regs[instr.reg] == 1 ? instr.param : 1)
	}
	println regs
}
