def main(String input, long base) {
	def ranges = input.split('\n')
		.collect {
			def split = it.split('-')
			new Range(from: split[0] as long, to: split[1] as long)
		}
    int help = 0
	while(ranges.size() > 1){
        ranges = help % 2 == 0 ? ranges : ranges.reverse()
		def minimized = ranges[1..-1].inject([ranges[0]] as Set){acc, cur ->
			acc.collectMany {it.combine(cur)} as Set
		}.sort()
		if(minimized == ranges){
			ranges = minimized
			break	
		}
		ranges = minimized
        ++help
	}
    println ranges
	return ranges.first().to + 1
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
