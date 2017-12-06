def text = new File('input.txt').text
List blocks = text.split('\t').collect {it as int}
println blocks
