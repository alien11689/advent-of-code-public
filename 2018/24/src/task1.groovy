static String readInput(String file) {
    return new File(file).text
}

static List<String> readInputAsLines(String file) {
    return readInput(file).trim().split('\n')
}

String input = readInput('input.txt').trim()
//input = readInput('other.txt').trim()

List<String> lines = readInputAsLines('input.txt')
//lines = readInputAsLines('other.txt')
