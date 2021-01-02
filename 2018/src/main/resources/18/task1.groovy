static String readInput(String file) {
    return new File(file).text
}

static List<String> readInputAsLines(String file) {
    return readInput(file).trim().split('\n')
}

List<String> lines = readInputAsLines('input.txt')
//lines = readInputAsLines('other.txt')

List<List<String>> board = lines.collect { it.collect() }

List<List<Integer>> neighbours(int x, int y, List<List<String>> board) {
    return [
            [x - 1, y],
            [x + 1, y],
            [x, y - 1],
            [x, y + 1],

            [x - 1, y - 1],
            [x - 1, y + 1],
            [x + 1, y + 1],
            [x + 1, y - 1],
    ].findAll { it[0] >= 0 && it[0] < board[0].size() && it[1] >= 0 && it[1] < board.size() }
}

def printBoard(List<List<String>> board) {
    println(board.collect { it.join() }.join('\n'))
}

//printBoard(board)
int tick = 0
while (tick < 10) {
    ++tick
    List<List<String>> newBoard = board.collect { it.collect() }
    for (int y = 0; y < board.size(); ++y) {
        for (int x = 0; x < board[0].size(); ++x) {
            List<String> adjacent = neighbours(x, y, board).collect { board[it[1]][it[0]] }
            if (board[y][x] == '.') {
                if (adjacent.count { it == '|' } >= 3) {
                    newBoard[y][x] = '|'
                }
            } else if (board[y][x] == '|') {
                if (adjacent.count { it == '#' } >= 3) {
                    newBoard[y][x] = '#'
                }
            } else {
                if (adjacent.count { it == '#' } >= 1 && adjacent.count { it == '|' } >= 1) {
                    // nothing
                } else {
                    newBoard[y][x] = '.'
                }
            }
        }
    }
    board = newBoard

}

List<String> flat = board.flatten()
int woods = flat.count { it == '|' }
int lumb = flat.count { it == '#' }
//println("Woods: $woods")
//println("Lumbs: $lumb")
println("Multi: ${lumb * woods}")
