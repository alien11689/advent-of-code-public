def main(String input) {
    new File(input).text
        .split('\n')
	.collect {println it; decompress( it) }
	.collect {println it; it}
        .sum()
}

long decompress(String text){
	long sum = 0
//	println "Partial: $text"
	for(int i = 0; i < text.size(); ){
//		println "i = $i"
		if(text[i] == '('){
			int ruleEnd = text.indexOf(')', i + 1)
//			println "RUle End $ruleEnd -> ${text[i..ruleEnd]}"
			Rule rule = createRule(text[i..ruleEnd])
			sum += rule.repeat * decompress(text[(ruleEnd + 1)..(ruleEnd+rule.chars)])
			i = ruleEnd+rule.chars + 1
		} else {
			sum += 1
			++i
		}	
	}
	return sum
}

@groovy.transform.Immutable
class Rule {
	int chars
	int repeat	
}

def createRule(String rule){
//		println "Rule: $rule"
	String[] split = rule.split(/[\(\)x]/)
	new Rule(split[1] as int, split[2] as int)
}

println(main('sample2.txt'))
println()
println(main('input.txt'))
