def text = new File('input.txt').text

List lines = text.split('\n').collect {it.trim()}

Map registers = [:]

class Instruction {
	String register
	boolean inc
	int amount
	Condition condition
}

class Condition {
	String register
	String sign
	int amount
}

List instructions = lines.split(' ').collect {
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

println instructions
