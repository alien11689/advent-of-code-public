def main(String input) {
    List<Map<String, Integer>> occurs = [[:],[:],[:],[:],[:],[:],[:],[:]]
    new File(input).text
        .split('\n')
        .each {
            it.eachWithIndex { e, i ->
	    	Map m = occurs[i]
		m[e] = (m[e]?:0) +1 
	    }
        }
	occurs.collect {
		def max = it.values().max()
		it.find {it.value == max}?.key
	}.join('')
}

println(main('sample.txt'))
println()
println(main('input.txt'))
