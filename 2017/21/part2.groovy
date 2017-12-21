String text = new File('input.txt').text
//text = new File('input2.txt').text
//text = new File('input3.txt').text

List lines = text.split('\n').collect {it.trim()}

List image = [".#." as List,  "..#" as List, '###' as List]

@groovy.transform.Immutable
class Rule {
	int size
	Set inputs
	List output

	def match(image){
		return image.size() == size && image in inputs
	}
}

def rotate(List grid) {
  List matrix = grid.collect {it.collect {it}}
  int length = matrix.size()-1;
  
  for (int i = 0; i <= (length)/2; i++) {
      for (int j = i; j < length-i; j++) {
       
       //Coordinate 1
       String p1 = matrix[i][j];
       
       //Coordinate 2
       String p2 = matrix[j][length-i];
       
       //Coordinate 3
       String p3 = matrix[length-i][length-j];
       
       //Coordinate 4
       String p4 = matrix[length-j][i];
       
       //Swap values of 4 coordinates.
       matrix[j][length-i] = p1;
       matrix[length-i][length-j] = p2;
       matrix[length-j][i] = p3;
       matrix[i][j] = p4;
      }
  }
  return matrix
 }

def rules = lines.collect {line -> 
	def (ins, out) = line.split(' => ')
	List input = (ins.split('/') as List).collect {it as List}
	List output = (out.split('/') as List).collect {it as List}
	Set inputs = [
		input,
		input.collect {it.reverse()},
		input.transpose().collect {it.reverse()}.transpose(),
	].collectMany {
		[it, rotate(it), rotate(rotate(it)), rotate(rotate(rotate(it)))]
	} as Set
	return new Rule(
		output.size() - 1,
		inputs,
		output
	)
}

def printGrid(it){
	println it.collect {it.join('')}.join('\n')
}

/**
rules.each {it
	println "Rule:"
	it.inputs.each {
		println "From: "
		printGrid(it)
	}
	println "To:"
	printGrid(it.output)

	println()
}
*/

int iter = 0
while(iter < 18){
	++iter
	println "Iter $iter"
	println image.collect {it.join('')}.join('\n')
	def newImage = []
	int split = image.size() % 2 == 0 ? 2 : 3
	for (int i = 0; i < image.size(); i += split) {
		List parts = []
		for (int j = 0; j < image.size(); j += split) { 
			List part = (i..<(i + split)).collect {it
				image[it][j..<(j+split)]
			}
			def rule = rules.find {it.match(part)}
			assert rule != null
			List result = rule.output
			parts << result
		}
		newImage.addAll(parts.inject(parts[0].size() == 3 ? [[], [], []] : [[],[],[],[]]) { acc, cur -> 
				(0..<(cur.size())).each {
					acc[it].addAll(cur[it])
				}
				return	acc
		})
	}
	image = newImage
}

println image.collect {it.join('')}.join('\n')
println image.collectMany {it}.findAll {it == '#'}.size()
