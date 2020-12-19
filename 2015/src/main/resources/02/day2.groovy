List<List<Integer>> input = new File('day2.data').text.split('\n').collect {it.split('x').collect {it as int}}

println input.collect {
    it.sort()
}.collect{
    it[0..1].sum() * 2 + it.inject(1) {a,b -> a*b}
}.sum()
