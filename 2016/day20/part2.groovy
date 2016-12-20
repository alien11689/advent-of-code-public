def main(String input, long base) {
	def ranges = input.split('\n')
		.collect {
			def split = it.split('-')
			new Range(from: split[0] as long, to: split[1] as long)
		}
	int help = 0
	while(ranges.size() > 1){
		println "Ranges size: ${ranges.size()}"
		def init = help % 2 == 0 ? ranges : ranges.reverse()
		def minimized = init.inject([init.first()] as Set){acc, cur ->
				acc.collectMany {it.combine(cur)} as Set
			}
		minimized = minimized.sort()
		if(minimized == ranges){
			ranges = minimized
			break	
		}
		ranges = minimized
		++help
	}
	println ranges
	long sum = 0
	for(int i =0; i < ranges.size() - 1; ++i){
		long diff = ranges[i+1].from - ranges[i].to - 1
		if(diff < 0){
			throw new RuntimeException(	"${ranges[i]} ${ranges[i+1]}")
		}
		sum += diff 
	}
	return sum
}

@groovy.transform.Sortable
@groovy.transform.ToString
@groovy.transform.EqualsAndHashCode
class Range {
	long from
	long to

	def combine(Range other){
		if(other.to + 1 == from || to + 1 == other.from){
			return [new Range(
				from: [from, other.from].min(),
				to: [to, other.to].max()
			)]
		}
		if(other.to < from || to < other.from){
			return [this,other]
		}
		return [new Range(
			from: [from, other.from].min(),
			to: [to, other.to].max()
		)]
	}
}

println(main(new File('sample.txt').text, 9))
println()
println(main(new File('input.txt').text, 4294967295))
