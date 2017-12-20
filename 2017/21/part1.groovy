String text = new File('input.txt').text

List lines = text.split('\n').collect {it.trim()}

List image = [".#." as List,  "..#" as List, '###' as List]

class Rule {
	List<List> inputs
	List outputs
}

def rules = lines.collect {
	
}
