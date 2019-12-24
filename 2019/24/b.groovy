import groovy.transform.Immutable

private List<String> readInput(String input) {
    new File(input).text.split('\n')
}

@Immutable
class Point {
    int x
    int y
    int level

    Set<Point> neighbours() {
        Set base = [
                new Point(x + 1, y, level),
                new Point(x - 1, y, level),
                new Point(x, y + 1, level),
                new Point(x, y - 1, level),
        ] as Set
        return base.collectMany { p ->
            if (p.x < 0) {
                [new Point(1, 2, level + 1)]
            } else if (p.x > 4) {
                [new Point(3, 2, level + 1)]
            } else if (p.y < 0) {
                [new Point(2, 1, level + 1)]
            } else if (p.y > 4) {
                [new Point(2, 3, level + 1)]
            } else if (p.x == 2 && p.y == 2) {
                if (x == 1) {
                    (0..4).collect {
                        new Point(0, it, level - 1)
                    }
                } else if (x == 3) {
                    (0..4).collect {
                        new Point(4, it, level - 1)
                    }
                } else if (y == 1) {
                    (0..4).collect {
                        new Point(it, 0, level - 1)
                    }
                } else {
                    (0..4).collect {
                        new Point(it, 4, level - 1)
                    }
                }
            } else {
                [p]
            }

        }
    }
}

Map<Point, Boolean> createBoard(List<String> input, int level) {
    Map m = [:].withDefault { false }
    for (int i = 0; i < input.size(); i++) {
        for (int j = 0; j < input[i].size(); j++) {
            m[new Point(j, i, level)] = (input[i][j] == '#')
        }
    }
    m.remove(new Point(2, 2, level))
    return m
}

Map<Point, Boolean> createEmpty(int level) {
    Map m = [:]
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
            m[new Point(j, i, level)] = false
        }
    }
    m.remove(new Point(2, 2, level))
    return m
}


Map<Point, Boolean> tick(Map<Point, Boolean> cur) {
    Set<Point> points = (cur.findAll { it.value }.keySet().collectMany { it.neighbours() } + cur.findAll { it.value }.keySet()) as Set
    Map<Point, Boolean> nextBoard = [:]
    points.each { p ->
        boolean exists = cur[p] ?: false
        int livingNeighbours = p.neighbours().findAll { it in cur && cur[it] }.size()
        if (exists) {
            if (livingNeighbours == 1) {
                nextBoard[p] = true
            } else {
                nextBoard[p] = false
            }
        } else {
            if (livingNeighbours in [1, 2]) {
                nextBoard[p] = true
            } else {
                nextBoard[p] = exists
            }
        }
    }
    nextBoard
}

//void printBoard(Map m) {
//    for (int i = 0; i < 5; ++i) {
//        for (int j = 0; j < 5; ++j) {
//            Point point = new Point(j, i)
//            if (point == Point.CENTER) {
//                print('?')
//            } else
//                print(m[point] ? '#' : '.')
//        }
//        println()
//    }
//    println('========================')
//}

String input = 'input.txt'
int minutes = 200
List<String> lines = readInput(input)

Map<Point, Boolean> board = createBoard(lines, 0)

//println(new Point(3, 2, 0).neighbours())

//Map<Point, Boolean> boards = createBoard(lines)
//printBoard(board)
int iter = 0
while (iter < minutes) {
    board = tick(board)
    ++iter
}
println(board.findAll { it.value }.size())