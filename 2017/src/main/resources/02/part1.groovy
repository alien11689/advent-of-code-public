def text = new File('input.txt').text

List lines = text.split('\n')
println lines.collect {it.split('\t').collect {it as int} }.collect { it.max() - it.min()}.sum()
