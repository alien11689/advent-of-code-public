def main(String input) {
    new File(input).text
        .split('\n')
	.collect {new Ip(it)}
	.findAll {it.hasTls()}
	.collect {1} 
	.sum()
}

@groovy.transform.ToString
class Ip {
	List<String> abba = []
	List<String> bridges = []
	Ip(String line){
		String[] split = line.split(/(\[|\])/)
		abba << split[0]
		for(int i = 1; i < split.size(); i+=2){
			bridges << split[i]
			abba << split[i+1]
		}
		println this
		println hasTls()
	}
	
	def hasPalindrom(String abba){
		abba.collect {it}.collate(4,1).findAll{it.size() == 4}.any {it[0] == it[3] && it[1] == it[2] && it[0] != it[1]}	
	}

	def hasTls(){
		abba.any {hasPalindrom(it)} && 
			!bridges.any { hasPalindrom(it)}
	}	
}

println(main('sample.txt'))
println()
println(main('input.txt'))
