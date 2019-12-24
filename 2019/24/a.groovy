import groovy.transform.Immutable

private List<String> readInput(String input) {
    new File(input).text.split('\n')
}

@Immutable
class Point {
    int x
    int y

    Set<Point> neighbours() {
        return [
                new Point(x + 1, y),
                new Point(x - 1, y),
                new Point(x, y + 1),
                new Point(x, y - 1),
        ] as Set
    }
}

Map<Point, Boolean> createBoard(List<String> input) {
    Map m = [:]
    for (int i = 0; i < input.size(); i++) {
        for (int j = 0; j < input[i].size(); j++) {
            m[new Point(j, i)] = (input[i][j] == '#')
        }
    }
    return m
}

Map<Point, Boolean> tick(Map<Point, Boolean> cur) {
    Map<Point, Boolean> next = [:]
    next = cur.collectEntries { e ->
        Point p = e.key
        boolean exists = e.value
        int livingNeighbours = p.neighbours().findAll { it in cur && cur[it] }.size()
        if (exists) {
            if (livingNeighbours == 1) {
                [(p): true]
            } else {
                [(p): false]
            }
        } else {
            if (livingNeighbours in [1, 2]) {
                [(p): true]
            } else {
                [(p): exists]
            }
        }
    }
    next
}

void printBoard(Map m) {
    for (int i = 0; i < 5; ++i) {
        for (int j = 0; j < 5; ++j) {
            print(m[new Point(j, i)] ? '#' : '.')
        }
        println()

    }
}

String input = 'input.txt'
List<String> lines = readInput(input)

Set memory = [] as Set
Map<Point, Boolean> board = createBoard(lines)
printBoard(board)
while (true) {
    if (board in memory) {
        break
    }
    memory << board
    board = tick(board)
    println('========================')
    printBoard(board)
}

printBoard(board)
long res = board.findAll { it.value }.collect {
    Point p = it.key
    int power = p.y * 5 + p.x
    (2**power) as long
}.sum()
println(res)