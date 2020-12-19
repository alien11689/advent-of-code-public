def main(String input, String expected) {
	def rules = input.split('\n')
		.collect {toRule(it)}
	(expected as List).permutations().sort().find {List init ->
		println "Checking $init"
		rules.inject(init){ cur, rule->
//			println "Cur: $cur, Rule: $rule"
			rule.apply(cur)
		}.join('') == expected
	}.join('')

}

def toRule(String inp){
	def s = inp.split(' ')
	if(inp.startsWith('swap position')){
		return new SwapPosition(from: s[2] as int, to: s[5] as int)	
	}
	if(inp.startsWith ('swap letter')){
		return new SwapLetter(from: s[2] as char, to: s[5] as char)	
	}
	if(inp.startsWith ('rotate left')){
		return new Rotate(amount: -(s[2] as int))	
	}
	if(inp.startsWith ('rotate right')){
		return new Rotate(amount: s[2] as int)
	}
	if(inp.startsWith ('rotate based')){
		return new RotateBased(letter: s[6] as char)	
	}
	if(inp.startsWith ('reverse')){
		return new Reverse(from: s[2] as int, to: s[4] as int)	
	}
	if(inp.startsWith ('move')){
		return new Move(from: s[2] as int, to: s[5] as int)	
	}
	throw new RuntimeException('No rule')
}

trait Rule { List apply(List input) {return []}}

@groovy.transform.ToString
class SwapPosition implements Rule {
	int from
	int to	
	List apply(List input){
		char a = input[from]	
		char b = input[to]
		input[from] = b
		input[to] = a
		return input
	}
}

@groovy.transform.ToString
class SwapLetter implements Rule {
	char from
	char to	
	List apply(List input){
		input.collect { it == from ? to : (it == to ? from :it)}	
	}
}

@groovy.transform.ToString
class Rotate implements Rule {
	int amount
	List apply(List input){
		List out = input.collect {it}
		for(int i = 0; i < input.size(); ++i){
			out[(i + amount) % input.size()] = input[i]	
		}
		out
	}
}

@groovy.transform.ToString
class RotateBased implements Rule {
	char letter
	List apply(List input){
		int i
		for (i = 0; i < input.size(); ++i){
			if(input[i] == letter){
				break	
			}
		}
		new Rotate(amount: i >= 4 ? i+2:i+1).apply(input)
	}
}

@groovy.transform.ToString
class Reverse implements Rule {
	int from
	int to
	List apply(List input){
		List reversed = input[from..to].reverse()
		(from > 0 ? input[0..<from] : []) + reversed + (to < input.size() - 1 ? input[(to+1)..<(input.size())] : [])
	}
}

@groovy.transform.ToString
class Move implements Rule {
	int from
	int to
	List apply(List input){
		List out = input.collect {it}
		char a = input[from]
		out.remove(from)
		out.size() > to ? out.add( to, a) : out.add(a)
		out
	}
}

//println(main(new File('sample.txt').text, 'decab')) //expected abcde
//println()
//println(main(new File('input.txt').text, 'agcebfdh')) //expected abcdefgh
println(main(new File('input.txt').text, 'fbgdceah'))
