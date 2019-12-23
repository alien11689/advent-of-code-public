private List<List<String>> readInput(String input) {
    new File(input).text.split('\n').collect { it.split(' ') as List }
}

String input = 'input.txt'
List<List<String>> lines = readInput(input)
