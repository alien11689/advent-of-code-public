def text = new File('input.txt').text
//def text = new File('input2.txt').text

List lines = text.split('\n').collect {it.trim()}

Map registers = [:]
int highestEver = 0

@groovy.transform.ToString
class Instruction {
	String register
	boolean inc
	int amount
	Condition condition

	int apply(reg, highestEver){
		if(reg[register] == null) {
			println "Create register $register"
			reg[register] = 0
		}
		if(condition.match(reg)){
			if(inc){
				reg[register] += amount
			}else {
				reg[register] -= amount
			}
		}
		return reg[register] > highestEver ? reg[register] : highestEver
	}
}

@groovy.transform.ToString
class Condition {
	String register
	String sign
	int amount

	boolean match(reg){
		if(reg[register] == null) {
			println "Create register $register"
			reg[register] = 0
		}
		switch(sign){ 
			case "==": return reg[register] == amount
			case "!=": return reg[register] != amount
			case ">=": return reg[register] >= amount
			case "<=": return reg[register] <= amount
			case ">": return reg[register] > amount
			case "<": return reg[register] < amount
			default: throw new Exception(sign)
		}
	}
}

List instructions = lines.collect{it.split(' ')}.collect {
	new Instruction(
		register: it[0],
		inc: it[1] == 'inc',
		amount: it[2] as int,
		condition: new Condition(
			register: it[4],
			sign: it[5],
			amount: it[6] as int
		)
	)
}

instructions.each{
		highestEver = it.apply(registers, highestEver)
}
println registers
println "Max at the end ${registers.values().max()}"
println "Highest ever: $highestEver"
